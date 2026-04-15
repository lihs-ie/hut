import {
  type Article,
  type ArticleIdentifier,
  type ArticleRepository,
  type ArticleSlug,
  type Criteria,
  validateArticle,
} from "@shared/domains/articles";
import {
  aggregateNotFoundError,
  unexpectedError,
  type AggregateNotFoundError,
  type UnexpectedError,
} from "@shared/aspects/error";
import { asyncResult, err, ok } from "@shared/aspects/result";
import { Order, SortByField } from "@shared/domains/common/sort";
import type {
  FirestoreFieldFilter,
  FirestoreOrderBy,
  FirestoreRestClient,
} from "./client";
import type { JsonValue } from "./value-converter";
import { describeError, toStringArray, toTimeline } from "./mapping";

const ARTICLES_COLLECTION = "articles";

const toArticle = (
  fields: Record<string, JsonValue>,
): Article | null => {
  const identifier = fields.identifier;
  const title = fields.title;
  const content = fields.content;
  const excerpt = fields.excerpt;
  const slug = fields.slug;
  const status = fields.status;

  if (
    typeof identifier !== "string" ||
    typeof title !== "string" ||
    typeof content !== "string" ||
    typeof excerpt !== "string" ||
    typeof slug !== "string" ||
    typeof status !== "string"
  ) {
    return null;
  }

  const timeline = toTimeline(fields.timeline);
  if (timeline === null) {
    return null;
  }

  const tags = toStringArray(fields.tags);
  const images = toStringArray(fields.images);

  const result = validateArticle({
    identifier,
    title,
    content,
    excerpt,
    slug,
    status,
    tags,
    images,
    timeline,
  });

  if (result.isErr) {
    return null;
  }
  return result.unwrap();
};

const buildSearchFilters = (criteria: Criteria): FirestoreFieldFilter[] => {
  const filters: FirestoreFieldFilter[] = [];

  if (typeof criteria.slug === "string") {
    filters.push({
      field: "slug",
      op: "EQUAL",
      value: { stringValue: criteria.slug },
    });
  }

  if (typeof criteria.status === "string") {
    filters.push({
      field: "status",
      op: "EQUAL",
      value: { stringValue: criteria.status },
    });
  }

  if (Array.isArray(criteria.tags) && criteria.tags.length > 0) {
    filters.push({
      field: "tags",
      op: "ARRAY_CONTAINS_ANY",
      value: {
        arrayValue: {
          values: criteria.tags.map((tag) => ({ stringValue: tag })),
        },
      },
    });
  }

  return filters;
};

const buildSearchOrderBy = (criteria: Criteria): FirestoreOrderBy[] => {
  const sortByField = criteria.sortBy ?? SortByField.CREATED_AT;
  const direction =
    (criteria.order ?? Order.DESC) === Order.ASC ? "ASCENDING" : "DESCENDING";
  return [
    {
      field: `timeline.${sortByField}`,
      direction,
    },
  ];
};

const matchesFreeWord = (article: Article, freeWord: string): boolean => {
  const keyword = freeWord.toLowerCase();
  return (
    article.title.toLowerCase().includes(keyword) ||
    article.content.toLowerCase().includes(keyword) ||
    article.excerpt.toLowerCase().includes(keyword)
  );
};

export const createFirestoreRestArticleRepository = (
  client: FirestoreRestClient,
): ArticleRepository => {
  const find: ArticleRepository["find"] = (identifier: ArticleIdentifier) => {
    return asyncResult(
      (async () => {
        try {
          const document = await client.getDocument(
            `${ARTICLES_COLLECTION}/${identifier}`,
          );
          if (document === null) {
            return err<Article, AggregateNotFoundError<"Article"> | UnexpectedError>(
              aggregateNotFoundError(
                "Article",
                `Article ${identifier} not found.`,
              ),
            );
          }
          const article = toArticle(document);
          if (article === null) {
            return err<Article, AggregateNotFoundError<"Article"> | UnexpectedError>(
              unexpectedError(`Failed to validate Article ${identifier}.`),
            );
          }
          return ok<Article, AggregateNotFoundError<"Article"> | UnexpectedError>(
            article,
          );
        } catch (error) {
          return err<Article, AggregateNotFoundError<"Article"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const findBySlug: ArticleRepository["findBySlug"] = (slug: ArticleSlug) => {
    return asyncResult(
      (async () => {
        try {
          const documents = await client.runQuery({
            collectionId: ARTICLES_COLLECTION,
            where: [
              {
                field: "slug",
                op: "EQUAL",
                value: { stringValue: slug },
              },
            ],
            limit: 1,
          });
          if (documents.length === 0) {
            return err<Article, AggregateNotFoundError<"Article"> | UnexpectedError>(
              aggregateNotFoundError(
                "Article",
                `Article with slug ${slug} not found.`,
              ),
            );
          }
          const article = toArticle(documents[0]);
          if (article === null) {
            return err<Article, AggregateNotFoundError<"Article"> | UnexpectedError>(
              unexpectedError(`Failed to validate Article for slug ${slug}.`),
            );
          }
          return ok<Article, AggregateNotFoundError<"Article"> | UnexpectedError>(
            article,
          );
        } catch (error) {
          return err<Article, AggregateNotFoundError<"Article"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const ofIdentifiers: ArticleRepository["ofIdentifiers"] = (
    identifiers: ArticleIdentifier[],
    throwOnMissing = false,
  ) => {
    return asyncResult(
      (async () => {
        try {
          const articles: Article[] = [];
          for (const identifier of identifiers) {
            const document = await client.getDocument(
              `${ARTICLES_COLLECTION}/${identifier}`,
            );
            if (document === null) {
              if (throwOnMissing) {
                return err<Article[], AggregateNotFoundError<"Article"> | UnexpectedError>(
                  aggregateNotFoundError(
                    "Article",
                    `Article ${identifier} not found.`,
                  ),
                );
              }
              continue;
            }
            const article = toArticle(document);
            if (article === null) {
              return err<Article[], AggregateNotFoundError<"Article"> | UnexpectedError>(
                unexpectedError(`Failed to validate Article ${identifier}.`),
              );
            }
            articles.push(article);
          }
          return ok<Article[], AggregateNotFoundError<"Article"> | UnexpectedError>(
            articles,
          );
        } catch (error) {
          return err<Article[], AggregateNotFoundError<"Article"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const search: ArticleRepository["search"] = (criteria: Criteria) => {
    return asyncResult(
      (async () => {
        try {
          const documents = await client.runQuery({
            collectionId: ARTICLES_COLLECTION,
            where: buildSearchFilters(criteria),
            orderBy: buildSearchOrderBy(criteria),
          });
          const articles: Article[] = [];
          for (const document of documents) {
            const article = toArticle(document);
            if (article !== null) {
              articles.push(article);
            }
          }

          const freeWord = criteria.freeWord;
          const filtered =
            typeof freeWord === "string"
              ? articles.filter((article) => matchesFreeWord(article, freeWord))
              : articles;
          return ok<Article[], UnexpectedError>(filtered);
        } catch (error) {
          return err<Article[], UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const persist: ArticleRepository["persist"] = () => {
    return asyncResult(
      Promise.resolve(
        err(unexpectedError("Article persist is not supported in read-only Reader.")),
      ),
    );
  };

  const terminate: ArticleRepository["terminate"] = () => {
    return asyncResult(
      Promise.resolve(
        err(unexpectedError("Article terminate is not supported in read-only Reader.")),
      ),
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
