import {
  AggregateNotFoundError,
  DuplicationError,
  unexpectedError,
  UnexpectedError,
  ValidationError,
} from "@/aspects/error";
import { AsyncResult, err, ok, Result } from "@/aspects/result";
import {
  Article,
  ArticleIdentifier,
  Criteria,
  UnvalidatedArticle,
  UnvalidatedCriteria,
} from "@/domains/articles";
import {
  ArticleArchivedEvent,
  ArticleDraftedEvent,
  ArticlePublishedEvent,
  ArticleTerminatedEvent,
  createArticleArchivedEvent,
  createArticleDraftedEvent,
  createArticlePublishedEvent,
  createArticleTerminatedEvent,
} from "@/domains/articles/event";
import { PublishStatus } from "@/domains/common";
import { Command } from "./common";
import { Logger } from "@/aspects/logger";

type ValidateIdentifier = (
  identifier: string
) => Result<ArticleIdentifier, ValidationError>;

type FindArticle = (
  identifier: ArticleIdentifier
) => AsyncResult<Article, AggregateNotFoundError<"Article">>;

export type ArticleFindWorkflow = (
  command: Command<{ identifier: string }>
) => AsyncResult<Article, ValidationError | AggregateNotFoundError<"Article">>;

export const createArticleFindWorkflow =
  (validate: ValidateIdentifier) =>
  (find: FindArticle) =>
  (logger: Logger): ArticleFindWorkflow =>
  (command: Command<{ identifier: string }>) => {
    logger.info("ArticleFindWorkflow started", {
      identifier: command.payload.identifier,
    });

    return validate(command.payload.identifier)
      .toAsync()
      .tap((identifier) => {
        logger.debug("Validation passed", { identifier });
      })
      .tapError((error) => {
        logger.warn("Validation failed", { error });
      })
      .andThen(find)
      .tap((article) => {
        logger.info("ArticleFindWorkflow completed", {
          identifier: article.identifier,
        });
      })
      .tapError((error) => {
        logger.error("ArticleFindWorkflow failed", { error });
      });
  };

type ValidateCriteria = (
  unvalidated: UnvalidatedCriteria
) => Result<Criteria, ValidationError[]>;

type SearchCriteria = (criteria: Criteria) => AsyncResult<Article[], never>;

export type ArticleSearchWorkflow = (
  command: Command<UnvalidatedCriteria>
) => AsyncResult<Article[], ValidationError[]>;

export const createArticleSearchWorkflow =
  (validate: ValidateCriteria) =>
  (search: SearchCriteria) =>
  (logger: Logger): ArticleSearchWorkflow =>
  (command: Command<UnvalidatedCriteria>) => {
    logger.info("ArticleSearchWorkflow started", { criteria: command.payload });

    return validate(command.payload)
      .toAsync()
      .tap((criteria) => {
        logger.debug("Criteria validation passed", { criteria });
      })
      .tapError((errors) => {
        logger.warn("Criteria validation failed", { errors });
      })
      .andThen(search)
      .tap((articles) => {
        logger.info("ArticleSearchWorkflow completed", {
          resultCount: articles.length,
        });
      })
      .tapError((error) => {
        logger.error("ArticleSearchWorkflow failed", { error });
      });
  };

type ValidateArticle = (
  unvalidated: UnvalidatedArticle
) => Result<Article, ValidationError[]>;

type PersistArticle = (
  article: Article
) => AsyncResult<
  void,
  AggregateNotFoundError<"Article"> | DuplicationError<"Article">
>;

type ArticlePersistedEvent =
  | ArticleDraftedEvent
  | ArticlePublishedEvent
  | ArticleArchivedEvent;

export type ArticlePersistWorkflow = (
  command: Command<UnvalidatedArticle>
) => AsyncResult<
  ArticlePersistedEvent,
  | AggregateNotFoundError<"Article">
  | DuplicationError<"Article">
  | ValidationError[]
  | UnexpectedError
>;

export const createArticlePersistWorkflow =
  (validate: ValidateArticle) =>
  (persist: PersistArticle) =>
  (logger: Logger): ArticlePersistWorkflow =>
  (command: Command<UnvalidatedArticle>) => {
    logger.info("ArticlePersistWorkflow started", {
      identifier: command.payload.identifier,
    });

    return validate(command.payload)
      .toAsync()
      .tap((article) => {
        logger.debug("Article validation passed", {
          identifier: article.identifier,
          status: article.status,
        });
      })
      .tapError((errors) => {
        logger.warn("Article validation failed", { errors });
      })
      .andThen((article) =>
        persist(article)
          .tap(() => {
            logger.debug("Article persisted", {
              identifier: article.identifier,
            });
          })
          .andThen((): Result<ArticlePersistedEvent, UnexpectedError> => {
            switch (article.status) {
              case PublishStatus.DRAFT:
                return ok(createArticleDraftedEvent(article.identifier));
              case PublishStatus.PUBLISHED:
                return ok(createArticlePublishedEvent(article.identifier));
              case PublishStatus.ARCHIVED:
                return ok(createArticleArchivedEvent(article.identifier));
              default:
                return err(
                  unexpectedError(
                    `Unknown publish status: ${article.status satisfies never}`
                  )
                );
            }
          })
      )
      .tap((event) => {
        logger.info("ArticlePersistWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("ArticlePersistWorkflow failed", { error });
      });
  };

type TerminateArticle = (
  identifier: ArticleIdentifier
) => AsyncResult<void, AggregateNotFoundError<"Article">>;

export type ArticleTerminateWorkflow = (
  command: Command<{ identifier: string }>
) => AsyncResult<
  ArticleTerminatedEvent,
  ValidationError | AggregateNotFoundError<"Article">
>;

export const createArticleTerminateWorkflow =
  (validate: ValidateIdentifier) =>
  (terminate: TerminateArticle) =>
  (logger: Logger): ArticleTerminateWorkflow =>
  (command: Command<{ identifier: string }>) => {
    logger.info("ArticleTerminateWorkflow started", {
      identifier: command.payload.identifier,
    });

    return validate(command.payload.identifier)
      .toAsync()
      .tap((identifier) => {
        logger.debug("Validation passed", { identifier });
      })
      .tapError((error) => {
        logger.warn("Validation failed", { error });
      })
      .andThen((identifier) =>
        terminate(identifier)
          .tap(() => {
            logger.debug("Article terminated", { identifier });
          })
          .map(() => createArticleTerminatedEvent(identifier))
      )
      .tap((event) => {
        logger.info("ArticleTerminateWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("ArticleTerminateWorkflow failed", { error });
      });
  };
