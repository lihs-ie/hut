import {
  AggregateNotFoundError,
  DuplicationError,
  ValidationError,
} from "@/aspects/error";
import { Logger } from "@/aspects/logger";
import { AsyncResult, Result } from "@/aspects/result";
import {
  Criteria,
  Memo,
  MemoIdentifier,
  UnvalidatedCriteria,
  UnvalidatedMemo,
} from "@/domains/memo";
import {
  createMemoCreatedEvent,
  createMemoTerminatedEvent,
  createMemoUpdatedEvent,
  MemoCreatedEvent,
  MemoTerminatedEvent,
  MemoUpdatedEvent,
} from "@/domains/memo/event";

type ValidateIdentifier = (
  identifier: string
) => Result<MemoIdentifier, ValidationError>;

type FindMemo = (
  identifier: MemoIdentifier
) => AsyncResult<Memo, AggregateNotFoundError<"Memo">>;

export type MemoFindWorkflow = (
  identifier: string
) => AsyncResult<Memo, ValidationError | AggregateNotFoundError<"Memo">>;

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

type ValidateCriteria = (
  unvalidated: UnvalidatedCriteria
) => Result<Criteria, ValidationError[]>;

type SearchCriteria = (criteria: Criteria) => AsyncResult<Memo[], never>;

export type MemoSearchWorkflow = (
  unvalidated: UnvalidatedCriteria
) => AsyncResult<Memo[], ValidationError[]>;

export const createMemoSearchWorkflow =
  (validate: ValidateCriteria) =>
  (search: SearchCriteria) =>
  (logger: Logger): MemoSearchWorkflow =>
  (unvalidated: UnvalidatedCriteria) => {
    logger.info("MemoSearchWorkflow started", { criteria: unvalidated });

    return validate(unvalidated)
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
  unvalidated: UnvalidatedMemo
) => Result<Memo, ValidationError[]>;

type PersistMemo = (
  memo: Memo
) => AsyncResult<
  void,
  AggregateNotFoundError<"Memo"> | DuplicationError<"Memo">
>;

type MemoPersistedEvent = MemoCreatedEvent | MemoUpdatedEvent;

export type MemoPersistWorkflow = (
  unvalidated: UnvalidatedMemo,
  isNew: boolean
) => AsyncResult<
  MemoPersistedEvent,
  AggregateNotFoundError<"Memo"> | DuplicationError<"Memo"> | ValidationError[]
>;

export const createMemoPersistWorkflow =
  (validate: ValidateMemo) =>
  (persist: PersistMemo) =>
  (logger: Logger): MemoPersistWorkflow =>
  (unvalidated: UnvalidatedMemo, isNew: boolean) => {
    logger.info("MemoPersistWorkflow started", {
      identifier: unvalidated.identifier,
      isNew,
    });

    return validate(unvalidated)
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
            logger.debug("Memo persisted", { identifier: memo.identifier });
          })
          .map(() =>
            isNew
              ? createMemoCreatedEvent(memo.identifier)
              : createMemoUpdatedEvent(memo.identifier)
          )
      )
      .tap((event) => {
        logger.info("MemoPersistWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("MemoPersistWorkflow failed", { error });
      });
  };

type TerminateMemo = (
  identifier: MemoIdentifier
) => AsyncResult<void, AggregateNotFoundError<"Memo">>;

export type MemoTerminateWorkflow = (
  identifier: string
) => AsyncResult<
  MemoTerminatedEvent,
  ValidationError | AggregateNotFoundError<"Memo">
>;

export const createMemoTerminateWorkflow =
  (validate: ValidateIdentifier) =>
  (terminate: TerminateMemo) =>
  (logger: Logger): MemoTerminateWorkflow =>
  (identifier: string) => {
    logger.info("MemoTerminateWorkflow started", { identifier });

    return validate(identifier)
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
          .map(() => createMemoTerminatedEvent(id))
      )
      .tap((event) => {
        logger.info("MemoTerminateWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("MemoTerminateWorkflow failed", { error });
      });
  };
