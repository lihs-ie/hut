import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
} from "firebase/firestore";
import {
  createSlugIndexInTransaction,
  createVersion,
  deleteSlugIndexInTransaction,
  FirestoreOperations,
  mapFirestoreError,
  updateSlugIndexInTransaction,
  Version,
} from "./common";
import {
  Article,
  ArticleIdentifier,
  ArticleRepository,
  ArticleSlug,
  Criteria,
  validateArticle,
} from "@shared/domains/articles";
import { fromPromise } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  duplicationError,
} from "@shared/aspects/error";

type PersistedArticle = {
  identifier: string;
  title: string;
  content: string;
  excerpt: string;
  slug: string;
  status: string;
  tags: string[];
  timeline: {
    createdAt: string;
    updatedAt: string;
  };
  version: number;
};

export const FirebaseArticleRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
): ArticleRepository => {
  const mapError = mapFirestoreError("Article");
  const versions: Map<ArticleIdentifier, Version> = new Map();

  const collection = operations
    .collection(firestore, "articles")
    .withConverter<Article, PersistedArticle>({
      toFirestore: (article: Article): PersistedArticle => {
        const currentVersion = versions.get(article.identifier);

        return {
          identifier: article.identifier,
          title: article.title,
          content: article.content,
          excerpt: article.excerpt,
          slug: article.slug,
          status: article.status,
          tags: [...article.tags],
          timeline: {
            createdAt: article.timeline.createdAt.toISOString(),
            updatedAt: article.timeline.updatedAt.toISOString(),
          },
          version: currentVersion?.increment().value ?? 1,
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedArticle>,
        options?: SnapshotOptions,
      ): Article => {
        const data = snapshot.data(options);

        const article = validateArticle({
          identifier: data.identifier,
          title: data.title,
          content: data.content,
          excerpt: data.excerpt,
          slug: data.slug,
          status: data.status,
          tags: data.tags,
          timeline: {
            createdAt: new Date(data.timeline.createdAt),
            updatedAt: new Date(data.timeline.updatedAt),
          },
        }).unwrap();

        const version = createVersion(data.version);

        versions.set(article.identifier, version);

        return article;
      },
    });

  const persist: ArticleRepository["persist"] = (article: Article) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, article.identifier);
        const snapshot = await transaction.get(document);
        const currentVersion = versions.get(article.identifier);

        if (currentVersion && currentVersion.value > 0) {
          if (!snapshot.exists()) {
            throw aggregateNotFoundError(
              "Article",
              `Article ${article.identifier} not found for update.`,
            );
          }

          const existingData = snapshot.data();
          const oldSlug = existingData.slug;

          await updateSlugIndexInTransaction(
            transaction,
            operations,
            firestore,
            {
              collectionName: "articles",
              oldSlug,
              newSlug: article.slug,
              referenceIdentifier: article.identifier,
              aggregateName: "Article",
            },
          );

          transaction.set(document, article, { merge: true });
          versions.set(article.identifier, currentVersion.increment());
        } else {
          if (snapshot.exists()) {
            throw duplicationError("Article", article.identifier);
          }

          await createSlugIndexInTransaction(
            transaction,
            operations,
            firestore,
            {
              collectionName: "articles",
              slug: article.slug,
              referenceIdentifier: article.identifier,
              aggregateName: "Article",
            },
          );

          transaction.set(document, article);
          versions.set(article.identifier, createVersion(1));
        }
      }),
      mapError,
    );
  };

  const find: ArticleRepository["find"] = (identifier: ArticleIdentifier) => {
    return fromPromise(
      (async () => {
        const document = operations.doc(collection, identifier);
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "Article",
            `Article ${identifier} not found.`,
          );
        }

        return snapshot.data();
      })(),
      mapError,
    );
  };

  const findBySlug: ArticleRepository["findBySlug"] = (slug: ArticleSlug) => {
    return fromPromise(
      (async () => {
        const query = operations.query(
          collection,
          operations.where("slug", "==", slug),
        );
        const querySnapshot = await operations.getDocs(query);

        if (querySnapshot.empty) {
          throw aggregateNotFoundError(
            "Article",
            `Article with slug ${slug} not found.`,
          );
        }

        const document = querySnapshot.docs[0];
        return document.data();
      })(),
      mapError,
    );
  };

  const ofIdentifiers: ArticleRepository["ofIdentifiers"] = (
    identifiers: ArticleIdentifier[],
    throwOnMissing = false,
  ) => {
    return fromPromise(
      (async () => {
        const articles: Article[] = [];

        for (const identifier of identifiers) {
          const document = operations.doc(collection, identifier);
          const snapshot = await operations.getDoc(document);

          if (!snapshot.exists()) {
            if (throwOnMissing) {
              throw aggregateNotFoundError(
                "Article",
                `Article ${identifier} not found.`,
              );
            }
            continue;
          }

          articles.push(snapshot.data());
        }

        return articles;
      })(),
      mapError,
    );
  };

  const search: ArticleRepository["search"] = (criteria: Criteria) => {
    return fromPromise(
      (async () => {
        const constraints = [];

        if (criteria.slug) {
          constraints.push(operations.where("slug", "==", criteria.slug));
        }

        if (criteria.status) {
          constraints.push(operations.where("status", "==", criteria.status));
        }

        if (criteria.tags && criteria.tags.length > 0) {
          constraints.push(
            operations.where("tags", "array-contains-any", criteria.tags),
          );
        }

        const q = operations.query(collection, ...constraints);
        const querySnapshot = await operations.getDocs(q);

        const articles: Article[] = [];
        querySnapshot.forEach((doc) => {
          articles.push(doc.data());
        });

        if (criteria.freeWord) {
          const keyword = criteria.freeWord.toLowerCase();
          return articles.filter(
            (article) =>
              article.title.toLowerCase().includes(keyword) ||
              article.content.toLowerCase().includes(keyword) ||
              article.excerpt.toLowerCase().includes(keyword),
          );
        }

        return articles;
      })(),
      mapError,
    );
  };

  const terminate: ArticleRepository["terminate"] = (
    identifier: ArticleIdentifier,
  ) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, identifier);
        const snapshot = await transaction.get(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "Article",
            `Article ${identifier} not found for deletion.`,
          );
        }

        const article = snapshot.data();
        deleteSlugIndexInTransaction(transaction, operations, firestore, {
          collectionName: "articles",
          slug: article.slug,
        });

        transaction.delete(document);
        versions.delete(identifier);
      }),
      mapError,
    );
  };

  return {
    persist,
    find,
    findBySlug,
    ofIdentifiers,
    search,
    terminate,
  };
};
