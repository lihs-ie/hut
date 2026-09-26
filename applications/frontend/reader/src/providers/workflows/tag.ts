import {
  validateCriteria,
  validateTagIdentifier,
  validateTagIdentifiers,
  validateTagNames,
} from "@shared/domains/attributes/tag";
import {
  createTagFindWorkflow,
  createTagOfIdentifiersWorkflow,
  createTagOfNamesWorkflow,
  createTagSearchWorkflow,
} from "@shared/workflows/attributes/tag";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { ReaderTagRepositoryProvider } from "@/providers/infrastructure/tag";

export const ReaderTagWorkflowProvider = {
  search: createTagSearchWorkflow(validateCriteria)(
    ReaderTagRepositoryProvider.firebase.search,
  )(LoggerProvider.console),

  find: createTagFindWorkflow(validateTagIdentifier)(
    ReaderTagRepositoryProvider.firebase.find,
  )(LoggerProvider.console),

  ofIdentifiers: createTagOfIdentifiersWorkflow(validateTagIdentifiers)(
    ReaderTagRepositoryProvider.firebase.ofIdentifiers,
  )(LoggerProvider.console),

  ofNames: createTagOfNamesWorkflow(validateTagNames)(
    ReaderTagRepositoryProvider.firebase.ofNames,
  )(LoggerProvider.console),
} as const;
