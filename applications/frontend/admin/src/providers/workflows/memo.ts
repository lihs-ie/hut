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
import { AdminMemoRepositoryProvider } from "../infrastructure/memo";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { validateSlug } from "@shared/domains/common";

export const AdminMemoWorkflowProvider = {
  find: createMemoFindWorkflow(validateMemoIdentifier)(
    AdminMemoRepositoryProvider.firebase.find,
  )(LoggerProvider.console),

  findBySlug: createMemoFindBySlugWorkflow(validateSlug)(
    AdminMemoRepositoryProvider.firebase.findBySlug,
  )(createPassthroughFilter())(LoggerProvider.console),

  search: createMemoSearchWorkflow(validateCriteria)(
    AdminMemoRepositoryProvider.firebase.search,
  )(LoggerProvider.console),

  create: createMemoCreateWorkflow(validateMemo)(
    AdminMemoRepositoryProvider.firebase.persist,
  )(LoggerProvider.console),

  edit: createMemoEditWorkflow(validateMemo)(
    AdminMemoRepositoryProvider.firebase.persist,
  )(LoggerProvider.console),

  addEntry: createPersistMemoEntryWorkflow(validateSlug)(validateEntry)(
    AdminMemoRepositoryProvider.firebase.findBySlug,
  )(AdminMemoRepositoryProvider.firebase.persist)(LoggerProvider.console),

  terminate: createMemoTerminateWorkflow(validateMemoIdentifier)(
    AdminMemoRepositoryProvider.firebase.terminate,
  )(LoggerProvider.console),
} as const;
