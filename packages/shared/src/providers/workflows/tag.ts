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
import { TagRepositoryProvider } from "../infrastructure/tag";
import { LoggerProvider } from "../infrastructure/logger";

export const TagWorkflowProvider = {
  search: createTagSearchWorkflow(validateCriteria)(
    TagRepositoryProvider.firebase.search,
  )(LoggerProvider.console),

  find: createTagFindWorkflow(validateTagIdentifier)(
    TagRepositoryProvider.firebase.find,
  )(LoggerProvider.console),

  ofIdentifiers: createTagOfIdentifiersWorkflow(validateTagIdentifiers)(
    TagRepositoryProvider.firebase.ofIdentifiers,
  )(LoggerProvider.console),

  ofNames: createTagOfNamesWorkflow(validateTagNames)(
    TagRepositoryProvider.firebase.ofNames,
  )(LoggerProvider.console),

  persist: createTagPersistWorkflow(validateTag)(
    TagRepositoryProvider.firebase.persist,
  )(LoggerProvider.console),

  terminate: createTagTerminateWorkflow(validateTagIdentifier)(
    TagRepositoryProvider.firebase.terminate,
  )(LoggerProvider.console),
} as const;
