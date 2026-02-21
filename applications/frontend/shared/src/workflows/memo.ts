import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { Logger } from "@shared/aspects/logger";
import { AsyncResult, combine, ok, Result } from "@shared/aspects/result";
import {
  addEntry,
  Criteria,
  Memo,
  MemoEntry,
  MemoIdentifier,
  MemoSlug,
  MemoSnapshot,
  toSnapshot,
  UnvalidatedCriteria,
  UnvalidatedEntry,
  UnvalidatedMemo,
} from "@shared/domains/memo";
import { ImageIdentifier } from "@shared/domains/image";
import {
  createMemoCreatedEvent,
  createMemoEditedEvent,
  createMemoTerminatedEvent,
  MemoCreatedEvent,
  MemoEditedEvent,
  MemoTerminatedEvent,
} from "@shared/domains/memo/event";
import { Command } from "./common";
import { ValidateSlug } from "@shared/domains/common";

type MemoFilter = (
  memo: Memo,
) => Result<Memo, AggregateNotFoundError<"Memo">>;

type ValidateIdentifier = (
  identifier: string,
) => Result<MemoIdentifier, ValidationError>;

type FindMemo = (
  identifier: MemoIdentifier,
) => AsyncResult<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>;

export type MemoFindWorkflow = (
  identifier: string,
) => AsyncResult<
  Memo,
  ValidationError | AggregateNotFoundError<"Memo"> | UnexpectedError
>;

export const createMemoFindWorkflow =
  (validate: ValidateIdentifier) =>
  (find: FindMemo) =>
  (logger: Logger): MemoFindWorkflow =>
  (identifier: string) => {
    logger.info("MemoFindWorkflow started", { identifier });

    return validate(identifier)
      .toAsync()
      .tap((id) => {
        logger.debug("Validation passed", { identifier: id });
      })
      .tapError((error) => {
        logger.warn("Validation failed", { error });
      })
      .andThen(find)
      .tap((memo) => {
        logger.info("MemoFindWorkflow completed", {
          identifier: memo.identifier,
        });
      })
      .tapError((error) => {
        logger.error("MemoFindWorkflow failed", { error });
      });
  };

type FindBySlug = (
  slug: MemoSlug,
) => AsyncResult<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>;

export type MemoFindBySlugWorkflow = (
  slug: string,
) => AsyncResult<
  Memo,
  ValidationError | AggregateNotFoundError<"Memo"> | UnexpectedError
>;

export const createMemoFindBySlugWorkflow =
  (validate: (slug: string) => Result<MemoSlug, ValidationError>) =>
  (findBySlug: FindBySlug) =>
  (filter: MemoFilter) =>
  (logger: Logger): MemoFindBySlugWorkflow =>
  (slug: string) => {
    logger.info("MemoFindBySlugWorkflow started", { slug });

    return validate(slug)
      .toAsync()
      .tap((validatedSlug) => {
        logger.debug("Validation passed", { slug: validatedSlug });
      })
      .tapError((error) => {
        logger.warn("Validation failed", { error });
      })
      .andThen(findBySlug)
      .andThen(filter)
      .tap((memo) => {
        logger.info("MemoFindBySlugWorkflow completed", {
          identifier: memo.identifier,
        });
      })
      .tapError((error) => {
        logger.error("MemoFindBySlugWorkflow failed", { error });
      });
  };

type ValidateCriteria = (
  unvalidated: UnvalidatedCriteria,
) => Result<Criteria, ValidationError[]>;

type SearchMemos = (criteria: Criteria) => AsyncResult<Memo[], UnexpectedError>;

type MemoSearchWorkflowCommand = Command<UnvalidatedCriteria>;

export type MemoSearchWorkflow = (
  command: MemoSearchWorkflowCommand,
) => AsyncResult<Memo[], ValidationError[] | UnexpectedError>;

export const createMemoSearchWorkflow =
  (validate: ValidateCriteria) =>
  (search: SearchMemos) =>
  (logger: Logger): MemoSearchWorkflow =>
  (command: MemoSearchWorkflowCommand) => {
    logger.info("MemoSearchWorkflow started", { command });

    return validate(command.payload)
      .toAsync()
      .tap((criteria) => {
        logger.debug("Criteria validation passed", { criteria });
      })
      .tapError((errors) => {
        logger.warn("Criteria validation failed", { errors });
      })
      .andThen(search)
      .tap((memos) => {
        logger.info("MemoSearchWorkflow completed", {
          resultCount: memos.length,
        });
      })
      .tapError((error) => {
        logger.error("MemoSearchWorkflow failed", { error });
      });
  };

type ValidateMemo = (
  unvalidated: UnvalidatedMemo,
) => Result<Memo, ValidationError[]>;

type PersistMemo = (
  memo: Memo,
) => AsyncResult<
  void,
  AggregateNotFoundError<"Memo"> | DuplicationError<"Memo"> | UnexpectedError
>;

type MemoCreateWorkflowCommand = Command<{
  unvalidated: UnvalidatedMemo;
}>;

export type MemoCreateWorkflow = (
  command: MemoCreateWorkflowCommand,
) => AsyncResult<
  MemoCreatedEvent,
  | AggregateNotFoundError<"Memo">
  | DuplicationError<"Memo">
  | ValidationError[]
  | UnexpectedError
>;

export const createMemoCreateWorkflow =
  (validate: ValidateMemo) =>
  (persist: PersistMemo) =>
  (logger: Logger): MemoCreateWorkflow =>
  (command: MemoCreateWorkflowCommand) => {
    logger.info("MemoCreateWorkflow started", {
      command,
    });

    return validate(command.payload.unvalidated)
      .toAsync()
      .tap((memo) => {
        logger.debug("Memo validation passed", {
          identifier: memo.identifier,
        });
      })
      .tapError((errors) => {
        logger.warn("Memo validation failed", { errors });
      })
      .andThen((memo) =>
        persist(memo)
          .tap(() => {
            logger.debug("Memo persisted", {
              identifier: memo.identifier,
            });
          })
          .map(() => createMemoCreatedEvent(toSnapshot(memo), command.now)),
      )
      .tap((event) => {
        logger.info("MemoCreateWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("MemoCreateWorkflow failed", { error });
      });
  };

export type MemoEditWorkflowCommand = Command<{
  unvalidated: UnvalidatedMemo;
  before: MemoSnapshot;
}>;

export type MemoEditWorkflow = (
  command: MemoEditWorkflowCommand,
) => AsyncResult<
  MemoEditedEvent,
  | AggregateNotFoundError<"Memo">
  | DuplicationError<"Memo">
  | ValidationError[]
  | UnexpectedError
>;

export const createMemoEditWorkflow =
  (validate: ValidateMemo) =>
  (persist: PersistMemo) =>
  (logger: Logger): MemoEditWorkflow =>
  (command: MemoEditWorkflowCommand) => {
    logger.info("MemoEditWorkflow started", {
      identifier: command.payload.unvalidated.identifier,
    });

    return validate(command.payload.unvalidated)
      .toAsync()
      .tap((memo) => {
        logger.debug("Memo validation passed", {
          identifier: memo.identifier,
        });
      })
      .tapError((errors) => {
        logger.warn("Memo validation failed", { errors });
      })
      .andThen((memo) =>
        persist(memo)
          .tap(() => {
            logger.debug("Memo persisted", {
              identifier: memo.identifier,
            });
          })
          .map(() =>
            createMemoEditedEvent(
              toSnapshot(memo),
              command.payload.before,
              command.now,
            ),
          ),
      )
      .tap((event) => {
        logger.info("MemoEditWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("MemoEditWorkflow failed", { error });
      });
  };

type PersistMemoEntryWorkflowCommand = Command<{
  slug: string;
  unvalidated: UnvalidatedEntry;
  images: ImageIdentifier[];
}>;

type PersistMemoEntryWorkflow = (
  command: PersistMemoEntryWorkflowCommand,
) => AsyncResult<
  MemoEditedEvent,
  | UnexpectedError
  | AggregateNotFoundError<"Memo">
  | ValidationError[]
  | DuplicationError<"Memo">
>;

type ValidateEntry = (
  unvalidated: UnvalidatedEntry,
) => Result<MemoEntry, ValidationError[]>;

export const createPersistMemoEntryWorkflow =
  (validateSlug: ValidateSlug) =>
  (validateEntry: ValidateEntry) =>
  (findBySlug: FindBySlug) =>
  (persist: PersistMemo) =>
  (logger: Logger): PersistMemoEntryWorkflow =>
  (command: PersistMemoEntryWorkflowCommand) => {
    logger.info("PersistMemoEntryWorkflow started", { command });

    return combine([
      validateSlug(command.payload.slug),
      validateEntry(command.payload.unvalidated),
    ] as const)
      .mapError((error) => (Array.isArray(error) ? error : [error]))
      .toAsync()
      .andThen(([slug, entry]) =>
        findBySlug(slug).andThen((memo) =>
          ok(addEntry(memo, entry, command.payload.images)),
        ),
      )
      .andThen((memo) => persist(memo).map(() => memo))
      .map((memo) =>
        createMemoEditedEvent(toSnapshot(memo), toSnapshot(memo), command.now),
      )
      .tap(() => {
        logger.info("PersistMemoEntryWorkflow completed", { command });
      })
      .tapError((error) => {
        logger.error("PersistMemoEntryWorkflow failed", { error });
      });
  };

type TerminateMemo = (
  identifier: MemoIdentifier,
) => AsyncResult<void, AggregateNotFoundError<"Memo"> | UnexpectedError>;

type MemoTerminateWorkflowCommand = Command<{
  identifier: string;
}>;

export type MemoTerminateWorkflow = (
  command: MemoTerminateWorkflowCommand,
) => AsyncResult<
  MemoTerminatedEvent,
  ValidationError | AggregateNotFoundError<"Memo"> | UnexpectedError
>;

export const createMemoTerminateWorkflow =
  (validate: ValidateIdentifier) =>
  (terminate: TerminateMemo) =>
  (logger: Logger): MemoTerminateWorkflow =>
  (command: MemoTerminateWorkflowCommand) => {
    logger.info("MemoTerminateWorkflow started", { command });

    return validate(command.payload.identifier)
      .toAsync()
      .tap((id) => {
        logger.debug("Validation passed", { identifier: id });
      })
      .tapError((error) => {
        logger.warn("Validation failed", { error });
      })
      .andThen((id) =>
        terminate(id)
          .tap(() => {
            logger.debug("Memo terminated", { identifier: id });
          })
          .map(() => createMemoTerminatedEvent(id)),
      )
      .tap((event) => {
        logger.info("MemoTerminateWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("MemoTerminateWorkflow failed", { error });
      });
  };
