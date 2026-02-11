import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
} from "firebase/firestore";
import {
  createVersion,
  FirestoreOperations,
  mapFirestoreError,
  Version,
} from "./common";
import {
  Criteria,
  Order,
  SearchIndex,
  SearchIndexIdentifier,
  SearchIndexRepository,
  Sort,
  validateSearchIndex,
} from "@shared/domains/search-index/common";
import { generateNgrams } from "@shared/aspects/ngram";
import { fromPromise } from "@shared/aspects/result";

type PersistedSearchIndex = {
  identifier: string;
  title: string;
  excerpt: string;
  tags: string[];
  reference: string;
  type: string;
  timeline: {
    createdAt: string;
    updatedAt: string;
  };
  ngrams: string[];
  version: number;
};

export const FirebaseSearchIndexRepository = (
  firestore: Firestore,
  operations: FirestoreOperations
): SearchIndexRepository => {
  const mapError = mapFirestoreError("SearchIndex");
  const versions: Map<SearchIndexIdentifier, Version> = new Map();

  const collection = operations
    .collection(firestore, "search-index")
    .withConverter<SearchIndex, PersistedSearchIndex>({
      toFirestore: (entry: SearchIndex): PersistedSearchIndex => {
        const currentVersion = versions.get(entry.identifier);

        return {
          identifier: entry.identifier,
          title: entry.title,
          excerpt: entry.excerpt,
          tags: entry.tags,
          type: entry.type,
          reference: entry.reference,
          timeline: {
            createdAt: entry.timeline.createdAt.toISOString(),
            updatedAt: entry.timeline.updatedAt.toISOString(),
          },
          ngrams: generateNgrams(
            [entry.title, entry.excerpt, ...entry.tags].join(" ")
          ),
          version: currentVersion?.increment().value ?? 1,
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedSearchIndex>,
        options?: SnapshotOptions
      ): SearchIndex => {
        const data = snapshot.data(options);

        const entry = validateSearchIndex({
          identifier: data.identifier,
          title: data.title,
          excerpt: data.excerpt,
          tags: data.tags,
          type: data.type,
          reference: data.reference,
          timeline: {
            createdAt: new Date(data.timeline.createdAt),
            updatedAt: new Date(data.timeline.updatedAt),
          },
        }).unwrap();

        const version = createVersion(data.version);

        versions.set(entry.identifier, version);

        return entry;
      },
    });

  const resolveOrderByField = (sortBy: Sort): string => {
    if (sortBy === Sort.LATEST) {
      return "timeline.updatedAt";
    }

    return "timeline.createdAt";
  };

  const resolveOrderDirection = (
    sortBy: Sort,
    order: Order
  ): "asc" | "desc" => {
    if (sortBy === Sort.OLDEST) {
      return order === Order.ASC ? "asc" : "desc";
    }

    return order === Order.ASC ? "asc" : "desc";
  };

  const search: SearchIndexRepository["search"] = (criteria: Criteria) => {
    return fromPromise(
      (async () => {
        const constraints = [];

        // Firestoreは1つのクエリで array-contains-any を1回しか使えない
        // タグ検索を優先し、freeWordはアプリケーション側でスコアリングに使用
        const hasTags = criteria.tags !== null && criteria.tags.length > 0;
        const hasFreeWord = criteria.freeWord !== null;

        if (hasFreeWord && !hasTags) {
          const searchNgrams = generateNgrams(criteria.freeWord!);

          if (searchNgrams.length > 0) {
            const ngramsToSearch = searchNgrams.slice(0, 10);

            constraints.push(
              operations.where("ngrams", "array-contains-any", ngramsToSearch)
            );
          }
        }

        if (criteria.type !== null) {
          constraints.push(operations.where("type", "==", criteria.type));
        }

        if (hasTags) {
          const tagsToSearch = criteria.tags!.slice(0, 10);

          constraints.push(
            operations.where("tags", "array-contains-any", tagsToSearch)
          );
        }

        if (criteria.sortBy !== null && criteria.order !== null) {
          const orderByField = resolveOrderByField(criteria.sortBy);
          const orderDirection = resolveOrderDirection(
            criteria.sortBy,
            criteria.order
          );

          constraints.push(operations.orderBy(orderByField, orderDirection));
        }

        const query = operations.query(collection, ...constraints);
        const querySnapshot = await operations.getDocs(query);

        const entries: SearchIndex[] = [];

        querySnapshot.forEach((document) => {
          entries.push(document.data());
        });

        return entries;
      })(),
      mapError
    );
  };

  return {
    search,
  };
};
