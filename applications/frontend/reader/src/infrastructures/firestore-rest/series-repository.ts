import {
  type Series,
  type SeriesIdentifier,
  type SeriesRepository,
  type SeriesSlug,
  type Criteria,
  validateSeries,
} from "@shared/domains/series";
import {
  aggregateNotFoundError,
  unexpectedError,
  type AggregateNotFoundError,
  type DuplicationError,
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
import {
  describeError,
  toOptionalString,
  toStringArray,
  toTimeline,
} from "./mapping";

const SERIES_COLLECTION = "series";

const toSeries = (fields: Record<string, JsonValue>): Series | null => {
  const identifier = fields.identifier;
  const title = fields.title;
  const slug = fields.slug;
  const statusValue = fields.status;

  if (
    typeof identifier !== "string" ||
    typeof title !== "string" ||
    typeof slug !== "string"
  ) {
    return null;
  }

  const timeline = toTimeline(fields.timeline);
  if (timeline === null) {
    return null;
  }

  const tags = toStringArray(fields.tags);
  const chapters = toStringArray(fields.chapters);
  const subTitle = toOptionalString(fields.subTitle);
  const description =
    typeof fields.description === "string" ? fields.description : undefined;
  const cover = toOptionalString(fields.cover);
  const status = typeof statusValue === "string" ? statusValue : "published";

  const result = validateSeries({
    identifier,
    title,
    slug,
    tags,
    subTitle,
    description,
    cover,
    chapters,
    status,
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

const matchesFreeWord = (series: Series, freeWord: string): boolean => {
  const keyword = freeWord.toLowerCase();
  if (series.title.toLowerCase().includes(keyword)) {
    return true;
  }
  if (series.description !== undefined) {
    if (series.description.toLowerCase().includes(keyword)) {
      return true;
    }
  }
  if (series.subTitle !== null && series.subTitle !== undefined) {
    if (series.subTitle.toLowerCase().includes(keyword)) {
      return true;
    }
  }
  return false;
};

export const createFirestoreRestSeriesRepository = (
  client: FirestoreRestClient,
): SeriesRepository => {
  const find: SeriesRepository["find"] = (identifier: SeriesIdentifier) => {
    return asyncResult(
      (async () => {
        try {
          const document = await client.getDocument(
            `${SERIES_COLLECTION}/${identifier}`,
          );
          if (document === null) {
            return err<Series, AggregateNotFoundError<"Series"> | UnexpectedError>(
              aggregateNotFoundError(
                "Series",
                `Series ${identifier} not found.`,
              ),
            );
          }
          const series = toSeries(document);
          if (series === null) {
            return err<Series, AggregateNotFoundError<"Series"> | UnexpectedError>(
              unexpectedError(`Failed to validate Series ${identifier}.`),
            );
          }
          return ok<Series, AggregateNotFoundError<"Series"> | UnexpectedError>(
            series,
          );
        } catch (error) {
          return err<Series, AggregateNotFoundError<"Series"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const findBySlug: SeriesRepository["findBySlug"] = (slug: SeriesSlug) => {
    return asyncResult(
      (async () => {
        try {
          const documents = await client.runQuery({
            collectionId: SERIES_COLLECTION,
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
            return err<Series, AggregateNotFoundError<"Series"> | UnexpectedError>(
              aggregateNotFoundError(
                "Series",
                `Series with slug ${slug} not found.`,
              ),
            );
          }
          const series = toSeries(documents[0]);
          if (series === null) {
            return err<Series, AggregateNotFoundError<"Series"> | UnexpectedError>(
              unexpectedError(`Failed to validate Series for slug ${slug}.`),
            );
          }
          return ok<Series, AggregateNotFoundError<"Series"> | UnexpectedError>(
            series,
          );
        } catch (error) {
          return err<Series, AggregateNotFoundError<"Series"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const ofIdentifiers: SeriesRepository["ofIdentifiers"] = (
    identifiers: SeriesIdentifier[],
    throwOnMissing = false,
  ) => {
    return asyncResult(
      (async () => {
        try {
          const seriesList: Series[] = [];
          for (const identifier of identifiers) {
            const document = await client.getDocument(
              `${SERIES_COLLECTION}/${identifier}`,
            );
            if (document === null) {
              if (throwOnMissing) {
                return err<Series[], AggregateNotFoundError<"Series"> | UnexpectedError>(
                  aggregateNotFoundError(
                    "Series",
                    `Series ${identifier} not found.`,
                  ),
                );
              }
              continue;
            }
            const series = toSeries(document);
            if (series === null) {
              return err<Series[], AggregateNotFoundError<"Series"> | UnexpectedError>(
                unexpectedError(`Failed to validate Series ${identifier}.`),
              );
            }
            seriesList.push(series);
          }
          return ok<Series[], AggregateNotFoundError<"Series"> | UnexpectedError>(
            seriesList,
          );
        } catch (error) {
          return err<Series[], AggregateNotFoundError<"Series"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const search: SeriesRepository["search"] = (criteria: Criteria) => {
    return asyncResult(
      (async () => {
        try {
          const documents = await client.runQuery({
            collectionId: SERIES_COLLECTION,
            where: buildSearchFilters(criteria),
            orderBy: buildSearchOrderBy(criteria),
          });
          const seriesList: Series[] = [];
          for (const document of documents) {
            const series = toSeries(document);
            if (series !== null) {
              seriesList.push(series);
            }
          }

          const freeWord = criteria.freeWord;
          const filtered =
            typeof freeWord === "string"
              ? seriesList.filter((series) => matchesFreeWord(series, freeWord))
              : seriesList;
          return ok<Series[], UnexpectedError>(filtered);
        } catch (error) {
          return err<Series[], UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const persist: SeriesRepository["persist"] = () => {
    return asyncResult(
      Promise.resolve(
        err<
          void,
          | AggregateNotFoundError<"Series">
          | DuplicationError<"Series">
          | UnexpectedError
        >(
          unexpectedError(
            "Series persist is not supported in read-only Reader.",
          ),
        ),
      ),
    );
  };

  const terminate: SeriesRepository["terminate"] = () => {
    return asyncResult(
      Promise.resolve(
        err<void, AggregateNotFoundError<"Series"> | UnexpectedError>(
          unexpectedError(
            "Series terminate is not supported in read-only Reader.",
          ),
        ),
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
