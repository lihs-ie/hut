import {
  validateCriteria,
  validateSeries,
  validateSeriesIdentifier,
} from "@shared/domains/series";
import {
  createSeriesFindBySlugWorkflow,
  createSeriesFindWorkflow,
  createSeriesPersistWorkflow,
  createSeriesSearchWorkflow,
  createSeriesTerminateWithChaptersWorkflow,
  createSeriesTerminateWorkflow,
} from "@shared/workflows/series";
import { AdminSeriesRepositoryProvider } from "../infrastructure/series";
import { AdminChapterRepositoryProvider } from "../infrastructure/chapter";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { validateSlug } from "@shared/domains/common";

export const AdminSeriesWorkflowProvider = {
  find: createSeriesFindWorkflow(validateSeriesIdentifier)(
    AdminSeriesRepositoryProvider.firebase.find
  )(LoggerProvider.console),

  findBySlug: createSeriesFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console
  )(AdminSeriesRepositoryProvider.firebase.findBySlug),

  search: createSeriesSearchWorkflow(validateCriteria)(
    AdminSeriesRepositoryProvider.firebase.search
  )(LoggerProvider.console),

  persist: createSeriesPersistWorkflow(validateSeries)(
    AdminSeriesRepositoryProvider.firebase.persist
  )(LoggerProvider.console),

  terminate: createSeriesTerminateWorkflow(validateSeriesIdentifier)(
    AdminSeriesRepositoryProvider.firebase.terminate
  )(LoggerProvider.console),

  terminateWithChapters: createSeriesTerminateWithChaptersWorkflow(
    validateSeriesIdentifier,
  )(AdminSeriesRepositoryProvider.firebase.find)(
    AdminChapterRepositoryProvider.firebase.terminate,
  )(AdminSeriesRepositoryProvider.firebase.terminate)(LoggerProvider.console),
} as const;
