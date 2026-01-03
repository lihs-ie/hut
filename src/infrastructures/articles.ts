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
  Article,
  ArticleIdentifier,
  ArticleRepository,
  Criteria,
  validateArticle,
} from "@/domains/articles";
import { fromPromise } from "@/aspects/result";
import { aggregateNotFoundError, duplicationError } from "@/aspects/error";

type PersistedArticle = {
  identifier: string;
  title: string;
  content: string;
  excerpt: string;
  slug: string;
  status: string;
  tags: string[];
  version: number;
};

export const FirebaseArticleRepository = (
  firestore: Firestore,
  operations: FirestoreOperations
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
          version: currentVersion?.increment().value ?? 1,
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedArticle>,
        options?: SnapshotOptions
      ): Article => {
        const data = snapshot.data(options);

        const article = validateArticle(data).unwrap();

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
              `Article ${article.identifier} not found for update.`
            );
          }

          transaction.set(document, article, { merge: true });
        } else {
          if (snapshot.exists()) {
            throw duplicationError("Article", article.identifier);
          }

          transaction.set(document, article);
        }
      }),
      mapError
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
            `Article ${identifier} not found.`
          );
        }

        return snapshot.data();
      })(),
      mapError
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
            operations.where("tags", "array-contains-any", criteria.tags)
          );
        }

        const q = operations.query(collection, ...constraints);
        const querySnapshot = await operations.getDocs(q);

        const articles: Article[] = [];
        querySnapshot.forEach((doc) => {
          articles.push(doc.data());
        });

        // freeWord がある場合はクライアントサイドでフィルタリング
        if (criteria.freeWord) {
          const keyword = criteria.freeWord.toLowerCase();
          return articles.filter(
            (article) =>
              article.title.toLowerCase().includes(keyword) ||
              article.content.toLowerCase().includes(keyword) ||
              article.excerpt.toLowerCase().includes(keyword)
          );
        }

        return articles;
      })(),
      mapError
    );
  };

  const terminate: ArticleRepository["terminate"] = (
    identifier: ArticleIdentifier
  ) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, identifier);
        const snapshot = await transaction.get(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError(
            "Article",
            `Article ${identifier} not found for deletion.`
          );
        }

        transaction.delete(document);
      }),
      mapError
    );
  };

  return {
    persist,
    find,
    search,
    terminate,
  };
};
