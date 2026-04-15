import {
  type Memo,
  type MemoIdentifier,
  type MemoRepository,
  type MemoSlug,
  type Criteria,
  type UnvalidatedEntry,
  validateMemo,
} from "@shared/domains/memo";
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
import {
  describeError,
  isJsonObject,
  toStringArray,
  toTimeline,
} from "./mapping";

const MEMOS_COLLECTION = "memos";

const toEntries = (value: JsonValue): UnvalidatedEntry[] => {
  if (!Array.isArray(value)) {
    return [];
  }
  const entries: UnvalidatedEntry[] = [];
  for (const item of value) {
    if (!isJsonObject(item)) {
      continue;
    }
    const text = item.text;
    const createdAt = item.createdAt;
    if (typeof text !== "string" || typeof createdAt !== "string") {
      continue;
    }
    entries.push({
      text,
      createdAt: new Date(createdAt),
    });
  }
  return entries;
};

const toMemo = (fields: Record<string, JsonValue>): Memo | null => {
  const identifier = fields.identifier;
  const title = fields.title;
  const slug = fields.slug;
  const status = fields.status;

  if (
    typeof identifier !== "string" ||
    typeof title !== "string" ||
    typeof slug !== "string" ||
    typeof status !== "string"
  ) {
    return null;
  }

  const timeline = toTimeline(fields.timeline);
  if (timeline === null) {
    return null;
  }

  const entries = toEntries(fields.entries);
  const tags = toStringArray(fields.tags);
  const images = toStringArray(fields.images);

  const result = validateMemo({
    identifier,
    title,
    slug,
    entries,
    tags,
    images,
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

const matchesFreeWord = (memo: Memo, freeWord: string): boolean => {
  const keyword = freeWord.toLowerCase();
  if (memo.title.toLowerCase().includes(keyword)) {
    return true;
  }
  return memo.entries.some((entry) =>
    entry.text.toLowerCase().includes(keyword),
  );
};

export const createFirestoreRestMemoRepository = (
  client: FirestoreRestClient,
): MemoRepository => {
  const find: MemoRepository["find"] = (identifier: MemoIdentifier) => {
    return asyncResult(
      (async () => {
        try {
          const document = await client.getDocument(
            `${MEMOS_COLLECTION}/${identifier}`,
          );
          if (document === null) {
            return err<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>(
              aggregateNotFoundError(
                "Memo",
                `Memo ${identifier} not found.`,
              ),
            );
          }
          const memo = toMemo(document);
          if (memo === null) {
            return err<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>(
              unexpectedError(`Failed to validate Memo ${identifier}.`),
            );
          }
          return ok<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>(
            memo,
          );
        } catch (error) {
          return err<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const findBySlug: MemoRepository["findBySlug"] = (slug: MemoSlug) => {
    return asyncResult(
      (async () => {
        try {
          const documents = await client.runQuery({
            collectionId: MEMOS_COLLECTION,
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
            return err<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>(
              aggregateNotFoundError(
                "Memo",
                `Memo with slug ${slug} not found.`,
              ),
            );
          }
          const memo = toMemo(documents[0]);
          if (memo === null) {
            return err<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>(
              unexpectedError(`Failed to validate Memo for slug ${slug}.`),
            );
          }
          return ok<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>(
            memo,
          );
        } catch (error) {
          return err<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const ofIdentifiers: MemoRepository["ofIdentifiers"] = (
    identifiers: MemoIdentifier[],
    throwOnMissing = false,
  ) => {
    return asyncResult(
      (async () => {
        try {
          const memos: Memo[] = [];
          for (const identifier of identifiers) {
            const document = await client.getDocument(
              `${MEMOS_COLLECTION}/${identifier}`,
            );
            if (document === null) {
              if (throwOnMissing) {
                return err<Memo[], AggregateNotFoundError<"Memo"> | UnexpectedError>(
                  aggregateNotFoundError(
                    "Memo",
                    `Memo ${identifier} not found.`,
                  ),
                );
              }
              continue;
            }
            const memo = toMemo(document);
            if (memo === null) {
              return err<Memo[], AggregateNotFoundError<"Memo"> | UnexpectedError>(
                unexpectedError(`Failed to validate Memo ${identifier}.`),
              );
            }
            memos.push(memo);
          }
          return ok<Memo[], AggregateNotFoundError<"Memo"> | UnexpectedError>(
            memos,
          );
        } catch (error) {
          return err<Memo[], AggregateNotFoundError<"Memo"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const search: MemoRepository["search"] = (criteria: Criteria) => {
    return asyncResult(
      (async () => {
        try {
          const documents = await client.runQuery({
            collectionId: MEMOS_COLLECTION,
            where: buildSearchFilters(criteria),
            orderBy: buildSearchOrderBy(criteria),
          });
          const memos: Memo[] = [];
          for (const document of documents) {
            const memo = toMemo(document);
            if (memo !== null) {
              memos.push(memo);
            }
          }

          const freeWord = criteria.freeWord;
          const filtered =
            typeof freeWord === "string"
              ? memos.filter((memo) => matchesFreeWord(memo, freeWord))
              : memos;
          return ok<Memo[], UnexpectedError>(filtered);
        } catch (error) {
          return err<Memo[], UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const persist: MemoRepository["persist"] = () => {
    return asyncResult(
      Promise.resolve(
        err(
          unexpectedError("Memo persist is not supported in read-only Reader."),
        ),
      ),
    );
  };

  const terminate: MemoRepository["terminate"] = () => {
    return asyncResult(
      Promise.resolve(
        err(
          unexpectedError(
            "Memo terminate is not supported in read-only Reader.",
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
