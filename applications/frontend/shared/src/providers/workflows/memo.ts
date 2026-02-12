import {
  validateCriteria,
  validateEntry,
  validateMemo,
  validateMemoIdentifier,
} from "@shared/domains/memo";
import {
  createMemoFindWorkflow,
  createMemoFindBySlugWorkflow,
  createMemoSearchWorkflow,
  createMemoTerminateWorkflow,
  createPersistMemoEntryWorkflow,
  createMemoCreateWorkflow,
  createMemoEditWorkflow,
} from "@shared/workflows/memo";
import { createPassthroughFilter } from "@shared/workflows/common";
import { MemoRepositoryProvider } from "../infrastructure/memo";
import { LoggerProvider } from "../infrastructure/logger";
import { validateSlug } from "@shared/domains/common";

export const MemoWorkflowProvider = {
  find: createMemoFindWorkflow(validateMemoIdentifier)(
    MemoRepositoryProvider.firebase.find,
  )(LoggerProvider.console),

  findBySlug: createMemoFindBySlugWorkflow(validateSlug)(
    MemoRepositoryProvider.firebase.findBySlug,
  )(createPassthroughFilter())(LoggerProvider.console),

  search: createMemoSearchWorkflow(validateCriteria)(
    MemoRepositoryProvider.firebase.search,
  )(LoggerProvider.console),

  create: createMemoCreateWorkflow(validateMemo)(
    MemoRepositoryProvider.firebase.persist,
  )(LoggerProvider.console),

  edit: createMemoEditWorkflow(validateMemo)(
    MemoRepositoryProvider.firebase.persist,
  )(LoggerProvider.console),

  addEntry: createPersistMemoEntryWorkflow(validateSlug)(validateEntry)(
    MemoRepositoryProvider.firebase.findBySlug,
  )(MemoRepositoryProvider.firebase.persist)(LoggerProvider.console),

  terminate: createMemoTerminateWorkflow(validateMemoIdentifier)(
    MemoRepositoryProvider.firebase.terminate,
  )(LoggerProvider.console),
} as const;
