import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { AsyncResult, ok, Result } from "@shared/aspects/result";
import {
  Article,
  ArticleIdentifier,
  ArticleSnapshot,
  Criteria,
  toSnapshot,
  UnvalidatedArticle,
  UnvalidatedCriteria,
} from "@shared/domains/articles";
import {
  ArticleCreatedEvent,
  ArticleEditedEvent,
  ArticleTerminatedEvent,
  createArticleCreatedEvent,
  createArticleEditedEvent,
  createArticleTerminatedEvent,
} from "@shared/domains/articles/event";
import { Slug, ValidateSlug } from "@shared/domains/common";
import { Logger } from "@shared/aspects/logger";
import { Command } from "./common";

type ValidateIdentifier = (
  identifier: string,
) => Result<ArticleIdentifier, ValidationError>;

type FindArticle = (
  identifier: ArticleIdentifier,
) => AsyncResult<Article, AggregateNotFoundError<"Article"> | UnexpectedError>;

export type ArticleFindWorkflow = (
  command: Command<{ identifier: string }>,
) => AsyncResult<
  Article,
  ValidationError | AggregateNotFoundError<"Article"> | UnexpectedError
>;
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

export type ArticleFindBySlugWorkflow = (
  command: Command<{ slug: string }>,
) => AsyncResult<
  Article,
  ValidationError | AggregateNotFoundError<"Article"> | UnexpectedError
>;

type ArticleFindBySlugCommand = Command<{ slug: string }>;

type FindBySlug = (
  slug: Slug,
) => AsyncResult<Article, AggregateNotFoundError<"Article"> | UnexpectedError>;

export const createArticleFindBySlugWorkflow =
  (validateSlug: ValidateSlug) =>
  (logger: Logger) =>
  (findBySlug: FindBySlug): ArticleFindBySlugWorkflow =>
  (command: ArticleFindBySlugCommand) => {
    logger.info("ArticleFindBySlugWorkflow started", {
      slug: command.payload.slug,
    });

    return validateSlug(command.payload.slug)
      .toAsync()
      .tap((slug) => {
        logger.debug("Slug validation passed", { slug });
      })
      .tapError((error) => {
        logger.warn("Slug validation failed", { error });
      })
      .andThen(findBySlug)
      .tap((article) => {
        logger.info("ArticleFindBySlugWorkflow completed", {
          identifier: article.identifier,
        });
      })
      .tapError((error) => {
        logger.error("ArticleFindBySlugWorkflow failed", { error });
      });
  };

type ValidateCriteria = (
  unvalidated: UnvalidatedCriteria,
) => Result<Criteria, ValidationError[]>;

type Search = (criteria: Criteria) => AsyncResult<Article[], UnexpectedError>;

export type ArticleSearchWorkflow = (
  command: Command<UnvalidatedCriteria>,
) => AsyncResult<Article[], ValidationError[] | UnexpectedError>;

export const createArticleSearchWorkflow =
  (validate: ValidateCriteria) =>
  (search: Search) =>
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
  unvalidated: UnvalidatedArticle,
) => Result<Article, ValidationError[]>;

type PersistArticle = (
  article: Article,
) => AsyncResult<void, DuplicationError<"Article"> | UnexpectedError>;

type ArticleCreateWorkflowCommand = Command<UnvalidatedArticle>;

export type ArticleCreateWorkflow = (
  command: ArticleCreateWorkflowCommand,
) => AsyncResult<
  ArticleCreatedEvent,
  DuplicationError<"Article"> | ValidationError[] | UnexpectedError
>;

export const createArticleCreateWorkflow =
  (validate: ValidateArticle) =>
  (persist: PersistArticle) =>
  (logger: Logger): ArticleCreateWorkflow =>
  (command: ArticleCreateWorkflowCommand) => {
    logger.info("ArticleCreateWorkflow started", {
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
          .andThen(() => ok(article)),
      )
      .map((article) =>
        createArticleCreatedEvent(toSnapshot(article), command.now),
      )
      .tap((event) => {
        logger.info("ArticleCreateWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("ArticleCreateWorkflow failed", { error });
      });
  };

type ArticleEditWorkflowCommand = Command<{
  unvalidated: UnvalidatedArticle;
  before: ArticleSnapshot;
}>;

type ArticleEditWorkflow = (
  command: ArticleEditWorkflowCommand,
) => AsyncResult<
  ArticleEditedEvent,
  DuplicationError<"Article"> | ValidationError[] | UnexpectedError
>;

export const createArticleEditWorkflow =
  (validate: ValidateArticle) =>
  (persist: PersistArticle) =>
  (logger: Logger): ArticleEditWorkflow =>
  (command: ArticleEditWorkflowCommand) => {
    logger.info("ArticleEditWorkflow started", { command });

    return validate(command.payload.unvalidated)
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
          .andThen(() => ok(article)),
      )
      .map((article) =>
        createArticleEditedEvent(
          toSnapshot(article),
          command.payload.before,
          command.now,
        ),
      )
      .tap((event) => {
        logger.info("ArticleEditWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("ArticleEditWorkflow failed", { error });
      });
  };

type TerminateArticle = (
  identifier: ArticleIdentifier,
) => AsyncResult<void, AggregateNotFoundError<"Article"> | UnexpectedError>;

export type ArticleTerminateWorkflow = (
  command: Command<{ identifier: string }>,
) => AsyncResult<
  ArticleTerminatedEvent,
  ValidationError | AggregateNotFoundError<"Article"> | UnexpectedError
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
          .map(() => createArticleTerminatedEvent(identifier)),
      )
      .tap((event) => {
        logger.info("ArticleTerminateWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("ArticleTerminateWorkflow failed", { error });
      });
  };
