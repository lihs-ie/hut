import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
} from "firebase/firestore";
import { FirestoreOperations, mapFirestoreError } from "./common";
import {
  SearchToken,
  SearchTokenIdentifier,
  SearchReference,
  SearchTokenRepository,
  SearchReferenceIdentifier,
  validateSearchToken,
  validateSearchReference,
  Criteria,
} from "@shared/domains/search-token";
import { fromPromise } from "@shared/aspects/result";
import { aggregateNotFoundError } from "@shared/aspects/error";

type PersistedSearchToken = {
  identifier: string;
  type: string;
  value: string;
  createdAt: string;
  updatedAt: string;
};

type PersistedSearchReference = {
  identifier: {
    type: string;
    content: string;
  };
  score: number;
  updatedAt: string;
};

export const FirebaseSearchTokenRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
): SearchTokenRepository => {
  const mapError = mapFirestoreError("SearchToken");

  const tokenCollection = operations
    .collection(firestore, "search-tokens")
    .withConverter<Omit<SearchToken, "references">, PersistedSearchToken>({
      toFirestore: (
        token: Omit<SearchToken, "references">,
      ): PersistedSearchToken => {
        return {
          identifier: token.identifier,
          type: token.type,
          value: token.value,
          createdAt: token.timeline.createdAt.toISOString(),
          updatedAt: token.timeline.updatedAt.toISOString(),
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedSearchToken>,
        options?: SnapshotOptions,
      ): Omit<SearchToken, "references"> => {
        const data = snapshot.data(options);
        return {
          identifier: data.identifier,
          type: data.type,
          value: data.value,
          timeline: {
            createdAt: new Date(data.createdAt),
            updatedAt: new Date(data.updatedAt),
          },
        } as Omit<SearchToken, "references">;
      },
    });

  const createReferenceConverter = () => ({
    toFirestore: (reference: SearchReference): PersistedSearchReference => {
      return {
        identifier: {
          type: reference.identifier.type,
          content: reference.identifier.content,
        },
        score: reference.score,
        updatedAt: reference.updatedAt.toISOString(),
      };
    },
    fromFirestore: (
      snapshot: QueryDocumentSnapshot<PersistedSearchReference>,
      options?: SnapshotOptions,
    ): SearchReference => {
      const data = snapshot.data(options);
      return validateSearchReference({
        identifier: {
          type: data.identifier.type,
          content: data.identifier.content,
        },
        score: data.score,
        updatedAt: new Date(data.updatedAt),
      }).unwrap();
    },
  });

  const getRefsCollection = (tokenIdentifier: SearchTokenIdentifier) => {
    return operations
      .collection(firestore, "search-tokens", tokenIdentifier, "refs")
      .withConverter<
        SearchReference,
        PersistedSearchReference
      >(createReferenceConverter());
  };

  const createReferenceDocumentIdentifier = (
    identifier: SearchReferenceIdentifier,
  ): string => {
    return `${identifier.type}:${identifier.content}`;
  };

  const loadReferences = async (
    tokenIdentifier: SearchTokenIdentifier,
  ): Promise<SearchReference[]> => {
    const refsCollection = getRefsCollection(tokenIdentifier);
    const querySnapshot = await operations.getDocs(
      operations.query(refsCollection),
    );
    const references: SearchReference[] = [];
    querySnapshot.forEach((document) => {
      references.push(document.data());
    });
    return references;
  };

  const persist: SearchTokenRepository["persist"] = (token: SearchToken) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const tokenDocument = operations.doc(tokenCollection, token.identifier);
        const tokenSnapshot = await transaction.get(tokenDocument);

        if (tokenSnapshot.exists()) {
          transaction.update(tokenDocument, {
            updatedAt: token.timeline.updatedAt.toISOString(),
          });
        } else {
          const tokenWithoutReferences: Omit<SearchToken, "references"> = {
            identifier: token.identifier,
            type: token.type,
            value: token.value,
            timeline: token.timeline,
          } as Omit<SearchToken, "references">;
          transaction.set(tokenDocument, tokenWithoutReferences);
        }

        const refsCollection = getRefsCollection(token.identifier);

        const existingRefsSnapshot = await operations.getDocs(
          operations.query(refsCollection),
        );
        const existingRefIdentifiers = new Set<string>();
        existingRefsSnapshot.forEach((document) => {
          existingRefIdentifiers.add(document.id);
        });

        const newRefIdentifiers = new Set<string>();
        for (const reference of token.references) {
          const referenceDocumentIdentifier = createReferenceDocumentIdentifier(
            reference.identifier,
          );
          newRefIdentifiers.add(referenceDocumentIdentifier);
          const referenceDocument = operations.doc(
            refsCollection,
            referenceDocumentIdentifier,
          );
          transaction.set(referenceDocument, reference);
        }

        for (const existingIdentifier of existingRefIdentifiers) {
          if (!newRefIdentifiers.has(existingIdentifier)) {
            const referenceDocument = operations.doc(
              refsCollection,
              existingIdentifier,
            );
            transaction.delete(referenceDocument);
          }
        }
      }),
      mapError,
    );
  };

  const find: SearchTokenRepository["find"] = (
    identifier: SearchTokenIdentifier,
  ) => {
    return fromPromise(
      (async () => {
        const tokenDocument = operations.doc(tokenCollection, identifier);
        const tokenSnapshot = await operations.getDoc(tokenDocument);

        if (!tokenSnapshot.exists()) {
          throw aggregateNotFoundError(
            "SearchToken",
            `SearchToken not found: ${identifier}`,
          );
        }

        const tokenData = tokenSnapshot.data();
        const references = await loadReferences(identifier);

        return validateSearchToken({
          identifier: tokenData.identifier,
          references: references.map((reference) => ({
            identifier: {
              type: reference.identifier.type,
              content: reference.identifier.content,
            },
            score: reference.score,
            updatedAt: reference.updatedAt,
          })),
          type: tokenData.type,
          value: tokenData.value,
          timeline: {
            createdAt: tokenData.timeline.createdAt,
            updatedAt: tokenData.timeline.updatedAt,
          },
        }).unwrap();
      })(),
      mapError,
    );
  };

  const ofIdentifiers: SearchTokenRepository["ofIdentifiers"] = (
    identifiers: SearchTokenIdentifier[],
    throwOnMissing = false,
  ) => {
    return fromPromise(
      (async () => {
        const tokens: SearchToken[] = [];

        for (const identifier of identifiers) {
          const tokenDocument = operations.doc(tokenCollection, identifier);
          const tokenSnapshot = await operations.getDoc(tokenDocument);

          if (!tokenSnapshot.exists()) {
            if (throwOnMissing) {
              throw aggregateNotFoundError(
                "SearchToken",
                `SearchToken not found: ${identifier}`,
              );
            }
            continue;
          }

          const tokenData = tokenSnapshot.data();
          const references = await loadReferences(identifier);

          const token = validateSearchToken({
            identifier: tokenData.identifier,
            references: references.map((reference) => ({
              identifier: {
                type: reference.identifier.type,
                content: reference.identifier.content,
              },
              score: reference.score,
              updatedAt: reference.updatedAt,
            })),
            type: tokenData.type,
            value: tokenData.value,
            timeline: {
              createdAt: tokenData.timeline.createdAt,
              updatedAt: tokenData.timeline.updatedAt,
            },
          }).unwrap();

          tokens.push(token);
        }

        return tokens;
      })(),
      mapError,
    );
  };

  const search: SearchTokenRepository["search"] = (criteria: Criteria) => {
    return fromPromise(
      (async () => {
        const constraints = [operations.limit(criteria.limit ?? 50)];
        const query = operations.query(tokenCollection, ...constraints);
        const querySnapshot = await operations.getDocs(query);

        const tokens: SearchToken[] = [];
        for (const document of querySnapshot.docs) {
          const tokenData = document.data();
          const references = await loadReferences(
            tokenData.identifier as SearchTokenIdentifier,
          );

          const token = validateSearchToken({
            identifier: tokenData.identifier,
            references: references.map((reference) => ({
              identifier: {
                type: reference.identifier.type,
                content: reference.identifier.content,
              },
              score: reference.score,
              updatedAt: reference.updatedAt,
            })),
            type: tokenData.type,
            value: tokenData.value,
            timeline: {
              createdAt: tokenData.timeline.createdAt,
              updatedAt: tokenData.timeline.updatedAt,
            },
          }).unwrap();

          tokens.push(token);
        }

        return tokens;
      })(),
      mapError,
    );
  };

  const terminate: SearchTokenRepository["terminate"] = (
    identifier: SearchTokenIdentifier,
  ) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const tokenDocument = operations.doc(tokenCollection, identifier);
        const tokenSnapshot = await transaction.get(tokenDocument);

        if (!tokenSnapshot.exists()) {
          throw aggregateNotFoundError(
            "SearchToken",
            `SearchToken not found: ${identifier}`,
          );
        }

        const refsCollection = getRefsCollection(identifier);
        const refsSnapshot = await operations.getDocs(
          operations.query(refsCollection),
        );
        refsSnapshot.forEach((document) => {
          transaction.delete(document.ref);
        });

        transaction.delete(tokenDocument);
      }),
      mapError,
    );
  };

  return {
    persist,
    find,
    ofIdentifiers,
    search,
    terminate,
  };
};
