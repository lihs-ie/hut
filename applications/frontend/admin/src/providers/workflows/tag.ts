import {
  validateCriteria,
  validateTag,
  validateTagIdentifier,
  validateTagIdentifiers,
  validateTagNames,
} from "@shared/domains/attributes/tag";
import {
  createTagSearchWorkflow,
  createTagOfIdentifiersWorkflow,
  createTagOfNamesWorkflow,
  createTagFindWorkflow,
  createTagPersistWorkflow,
  createTagTerminateWorkflow,
} from "@shared/workflows/attributes/tag";
import { AdminTagRepositoryProvider } from "../infrastructure/tag";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";

export const AdminTagWorkflowProvider = {
  search: createTagSearchWorkflow(validateCriteria)(
    AdminTagRepositoryProvider.firebase.search,
  )(LoggerProvider.console),

  find: createTagFindWorkflow(validateTagIdentifier)(
    AdminTagRepositoryProvider.firebase.find,
  )(LoggerProvider.console),

  ofIdentifiers: createTagOfIdentifiersWorkflow(validateTagIdentifiers)(
    AdminTagRepositoryProvider.firebase.ofIdentifiers,
  )(LoggerProvider.console),

  ofNames: createTagOfNamesWorkflow(validateTagNames)(
    AdminTagRepositoryProvider.firebase.ofNames,
  )(LoggerProvider.console),

  persist: createTagPersistWorkflow(validateTag)(
    AdminTagRepositoryProvider.firebase.persist,
  )(LoggerProvider.console),

  terminate: createTagTerminateWorkflow(validateTagIdentifier)(
    AdminTagRepositoryProvider.firebase.terminate,
  )(LoggerProvider.console),
} as const;
