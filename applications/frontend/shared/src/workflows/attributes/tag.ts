import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { Logger } from "@shared/aspects/logger";
import { AsyncResult, ok, Result } from "@shared/aspects/result";
import {
  Criteria,
  Tag,
  TagIdentifier,
  TagName,
  TagPersistedEvent,
  TagTerminatedEvent,
  UnvalidatedCriteria,
  UnvalidatedTag,
} from "@shared/domains/attributes/tag";
import { Command } from "../common";
import { ulid } from "ulid";

export type ValidateIdentifier = (
  identifier: string,
) => Result<TagIdentifier, ValidationError>;

export type TagFindWorkflowCommand = Command<string>;

export type TagFindWorkflow = (
  command: TagFindWorkflowCommand,
) => AsyncResult<
  Tag,
  AggregateNotFoundError<"Tag"> | UnexpectedError | ValidationError
>;

type Find = (
  identifier: TagIdentifier,
) => AsyncResult<Tag, AggregateNotFoundError<"Tag"> | UnexpectedError>;

export const createTagFindWorkflow =
  (validate: ValidateIdentifier) =>
  (find: Find) =>
  (logger: Logger): TagFindWorkflow =>
  (command: TagFindWorkflowCommand) => {
    logger.info("TagFindWorkflow started", { identifier: command.payload });

    return validate(command.payload)
      .toAsync()
      .andThen(find)
      .tap((tag) => {
        logger.info("TagFindWorkflow completed", {
          tagIdentifier: tag.identifier,
        });
      })
      .tapError((error) => {
        logger.error("TagFindWorkflow failed", { error });
      });
  };

export type ValidateIdentifiers = (
  identifiers: string[],
) => Result<TagIdentifier[], ValidationError[]>;

export type OfIdentifiers = (
  identifiers: TagIdentifier[],
) => AsyncResult<Tag[], AggregateNotFoundError<"Tag"> | UnexpectedError>;

export type TagOfIdentifiersWorkflow = (
  identifiers: string[],
) => AsyncResult<
  Tag[],
  AggregateNotFoundError<"Tag"> | UnexpectedError | ValidationError[]
>;

export const createTagOfIdentifiersWorkflow =
  (validateIdentifiers: ValidateIdentifiers) =>
  (ofIdentifiers: OfIdentifiers) =>
  (logger: Logger): TagOfIdentifiersWorkflow =>
  (identifiers: string[]) => {
    logger.info("TagOfIdentifiersWorkflow started", { identifiers });

    return validateIdentifiers(identifiers)
      .toAsync()
      .tap((validIdentifiers) => {
        logger.debug("Validation passed", { validIdentifiers });
      })
      .tapError((errors) => {
        logger.warn("Validation failed", { errors });
      })
      .andThen(ofIdentifiers)
      .tap((tags) => {
        logger.info("TagOfIdentifiersWorkflow completed", {
          resultCount: tags.length,
        });
      })
      .tapError((error) => {
        logger.error("TagOfIdentifiersWorkflow failed", { error });
      });
  };

type ValidateCriteria = (
  candidate: UnvalidatedCriteria,
) => Result<Criteria, ValidationError[]>;

type SearchTag = (criteria: Criteria) => AsyncResult<Tag[], UnexpectedError>;

export type TagSearchWorkflow = (
  command: Command<UnvalidatedCriteria>,
) => AsyncResult<Tag[], UnexpectedError | ValidationError[]>;

export const createTagSearchWorkflow =
  (validateCriteria: ValidateCriteria) =>
  (search: SearchTag) =>
  (logger: Logger): TagSearchWorkflow =>
  (command: Command<UnvalidatedCriteria>) => {
    logger.info("TagSearchWorkflow started", { criteria: command.payload });

    return validateCriteria(command.payload)
      .toAsync()
      .tap((criteria) => {
        logger.debug("Criteria validation passed", { criteria });
      })
      .tapError((errors) => {
        logger.warn("Criteria validation failed", { errors });
      })
      .andThen(search)
      .tap((tags) => {
        logger.info("TagSearchWorkflow completed", {
          resultCount: tags.length,
        });
      })
      .tapError((error) => {
        logger.error("TagSearchWorkflow failed", { error });
      });
  };

type TagPersistWorkflowCommand = Command<UnvalidatedTag>;

export type TagPersistWorkflow = (
  command: TagPersistWorkflowCommand,
) => AsyncResult<
  TagPersistedEvent,
  UnexpectedError | ValidationError[] | DuplicationError<"Tag">
>;

type ValidateTag = (
  unvalidated: UnvalidatedTag,
) => Result<Tag, ValidationError[]>;

type PersistTag = (
  tag: Tag,
) => AsyncResult<void, UnexpectedError | DuplicationError<"Tag">>;

export const createTagPersistWorkflow =
  (validate: ValidateTag) =>
  (persist: PersistTag) =>
  (logger: Logger): TagPersistWorkflow =>
  (command: TagPersistWorkflowCommand) => {
    logger.info("TagPersistWorkflow started", { payload: command.payload });

    return validate(command.payload)
      .toAsync()
      .andThen((tag) => persist(tag).andThen(() => ok(tag.identifier)))
      .tap(() => {
        logger.info("TagPersistWorkflow completed");
      })
      .tapError((error) => {
        logger.error("TagPersistWorkflow failed", { error });
      })
      .map(
        (identifier): TagPersistedEvent => ({
          identifier: ulid(),
          occurredAt: new Date(),
          type: "tag.persisted",
          payload: { identifier },
        }),
      );
  };

export type TagTerminateWorkflowCommand = Command<string>;

export type TagTerminateWorkflow = (
  command: TagTerminateWorkflowCommand,
) => AsyncResult<
  TagTerminatedEvent,
  UnexpectedError | ValidationError | AggregateNotFoundError<"Tag">
>;

type TerminateTag = (
  identifier: TagIdentifier,
) => AsyncResult<void, UnexpectedError | AggregateNotFoundError<"Tag">>;

export const createTagTerminateWorkflow =
  (validate: ValidateIdentifier) =>
  (terminate: TerminateTag) =>
  (logger: Logger): TagTerminateWorkflow =>
  (command: TagTerminateWorkflowCommand) => {
    logger.info("TagTerminateWorkflow started", {
      identifier: command.payload,
    });

    return validate(command.payload)
      .toAsync()
      .andThen(terminate)
      .tap(() => {
        logger.info("TagTerminateWorkflow completed", {
          tagIdentifier: command.payload,
        });
      })
      .tapError((error) => {
        logger.error("TagTerminateWorkflow failed", { error });
      })
      .map(
        (): TagTerminatedEvent => ({
          identifier: ulid(),
          occurredAt: new Date(),
          type: "tag.terminated",
          payload: { identifier: command.payload as TagIdentifier },
        }),
      );
  };

export type ValidateNames = (
  names: string[],
) => Result<TagName[], ValidationError[]>;

export type OfNames = (names: TagName[]) => AsyncResult<Tag[], UnexpectedError>;

export type TagOfNamesWorkflow = (
  names: string[],
) => AsyncResult<Tag[], UnexpectedError | ValidationError[]>;

export const createTagOfNamesWorkflow =
  (validateNames: ValidateNames) =>
  (ofNames: OfNames) =>
  (logger: Logger): TagOfNamesWorkflow =>
  (names: string[]) => {
    logger.info("TagOfNamesWorkflow started", { names });

    return validateNames(names)
      .toAsync()
      .tap((validNames) => {
        logger.debug("Validation passed", { validNames });
      })
      .tapError((errors) => {
        logger.warn("Validation failed", { errors });
      })
      .andThen(ofNames)
      .tap((tags) => {
        logger.info("TagOfNamesWorkflow completed", {
          resultCount: tags.length,
        });
      })
      .tapError((error) => {
        logger.error("TagOfNamesWorkflow failed", { error });
      });
  };
