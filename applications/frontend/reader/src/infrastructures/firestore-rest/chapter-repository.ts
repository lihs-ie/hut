import {
  type Chapter,
  type ChapterIdentifier,
  type ChapterRepository,
  validateChapter,
} from "@shared/domains/series/chapter";
import type { Slug } from "@shared/domains/common";
import {
  aggregateNotFoundError,
  unexpectedError,
  type AggregateNotFoundError,
  type UnexpectedError,
} from "@shared/aspects/error";
import { asyncResult, err, ok } from "@shared/aspects/result";
import type { FirestoreRestClient } from "./client";
import type { JsonValue } from "./value-converter";
import { describeError, toStringArray, toTimeline } from "./mapping";

const CHAPTERS_COLLECTION = "chapters";

const toChapter = (fields: Record<string, JsonValue>): Chapter | null => {
  const identifier = fields.identifier;
  const title = fields.title;
  const slug = fields.slug;
  const content = fields.content;
  const statusValue = fields.status;

  if (
    typeof identifier !== "string" ||
    typeof title !== "string" ||
    typeof slug !== "string" ||
    typeof content !== "string"
  ) {
    return null;
  }

  const status = typeof statusValue === "string" ? statusValue : "published";
  const timeline = toTimeline(fields.timeline);
  if (timeline === null) {
    return null;
  }

  const images = toStringArray(fields.images);

  const result = validateChapter({
    identifier,
    title,
    slug,
    content,
    images,
    status,
    timeline,
  });

  if (result.isErr) {
    return null;
  }
  return result.unwrap();
};

export const createFirestoreRestChapterRepository = (
  client: FirestoreRestClient,
): ChapterRepository => {
  const find: ChapterRepository["find"] = (identifier: ChapterIdentifier) => {
    return asyncResult(
      (async () => {
        try {
          const document = await client.getDocument(
            `${CHAPTERS_COLLECTION}/${identifier}`,
          );
          if (document === null) {
            return err<Chapter, AggregateNotFoundError<"Chapter"> | UnexpectedError>(
              aggregateNotFoundError(
                "Chapter",
                `Chapter ${identifier} not found.`,
              ),
            );
          }
          const chapter = toChapter(document);
          if (chapter === null) {
            return err<Chapter, AggregateNotFoundError<"Chapter"> | UnexpectedError>(
              unexpectedError(`Failed to validate Chapter ${identifier}.`),
            );
          }
          return ok<Chapter, AggregateNotFoundError<"Chapter"> | UnexpectedError>(
            chapter,
          );
        } catch (error) {
          return err<Chapter, AggregateNotFoundError<"Chapter"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const findBySlug: ChapterRepository["findBySlug"] = (slug: Slug) => {
    return asyncResult(
      (async () => {
        try {
          const documents = await client.runQuery({
            collectionId: CHAPTERS_COLLECTION,
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
            return err<Chapter, AggregateNotFoundError<"Chapter"> | UnexpectedError>(
              aggregateNotFoundError(
                "Chapter",
                `Chapter with slug ${slug} not found.`,
              ),
            );
          }
          const chapter = toChapter(documents[0]);
          if (chapter === null) {
            return err<Chapter, AggregateNotFoundError<"Chapter"> | UnexpectedError>(
              unexpectedError(`Failed to validate Chapter for slug ${slug}.`),
            );
          }
          return ok<Chapter, AggregateNotFoundError<"Chapter"> | UnexpectedError>(
            chapter,
          );
        } catch (error) {
          return err<Chapter, AggregateNotFoundError<"Chapter"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const ofIdentifiers: ChapterRepository["ofIdentifiers"] = (
    identifiers: ChapterIdentifier[],
    throwOnMissing = false,
  ) => {
    return asyncResult(
      (async () => {
        try {
          const chapterList: Chapter[] = [];
          for (const identifier of identifiers) {
            const document = await client.getDocument(
              `${CHAPTERS_COLLECTION}/${identifier}`,
            );
            if (document === null) {
              if (throwOnMissing) {
                return err<Chapter[], AggregateNotFoundError<"Chapter"> | UnexpectedError>(
                  aggregateNotFoundError(
                    "Chapter",
                    `Chapter ${identifier} not found.`,
                  ),
                );
              }
              continue;
            }
            const chapter = toChapter(document);
            if (chapter === null) {
              return err<Chapter[], AggregateNotFoundError<"Chapter"> | UnexpectedError>(
                unexpectedError(`Failed to validate Chapter ${identifier}.`),
              );
            }
            chapterList.push(chapter);
          }
          return ok<Chapter[], AggregateNotFoundError<"Chapter"> | UnexpectedError>(
            chapterList,
          );
        } catch (error) {
          return err<Chapter[], AggregateNotFoundError<"Chapter"> | UnexpectedError>(
            unexpectedError(describeError(error), error),
          );
        }
      })(),
    );
  };

  const persist: ChapterRepository["persist"] = () => {
    return asyncResult(
      Promise.resolve(
        err(
          unexpectedError(
            "Chapter persist is not supported in read-only Reader.",
          ),
        ),
      ),
    );
  };

  const terminate: ChapterRepository["terminate"] = () => {
    return asyncResult(
      Promise.resolve(
        err(
          unexpectedError(
            "Chapter terminate is not supported in read-only Reader.",
          ),
        ),
      ),
    );
  };

  return {
    find,
    findBySlug,
    ofIdentifiers,
    persist,
    terminate,
  };
};
