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
import { createPassthroughFilter } from "@shared/workflows/common";
import { SeriesRepositoryProvider } from "@shared/providers/infrastructure/series";
import { ChapterRepositoryProvider } from "@shared/providers/infrastructure/chapter";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { validateSlug } from "@shared/domains/common";

export const AdminSeriesWorkflowProvider = {
  find: createSeriesFindWorkflow(validateSeriesIdentifier)(
    SeriesRepositoryProvider.firebase.find
  )(LoggerProvider.console),

  findBySlug: createSeriesFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console
  )(SeriesRepositoryProvider.firebase.findBySlug)(createPassthroughFilter()),

  search: createSeriesSearchWorkflow(validateCriteria)(
    SeriesRepositoryProvider.firebase.search
  )(LoggerProvider.console),

  persist: createSeriesPersistWorkflow(validateSeries)(
    SeriesRepositoryProvider.firebase.persist
  )(LoggerProvider.console),

  terminate: createSeriesTerminateWorkflow(validateSeriesIdentifier)(
    SeriesRepositoryProvider.firebase.terminate
  )(LoggerProvider.console),

  terminateWithChapters: createSeriesTerminateWithChaptersWorkflow(
    validateSeriesIdentifier,
  )(SeriesRepositoryProvider.firebase.find)(
    ChapterRepositoryProvider.firebase.terminate,
  )(SeriesRepositoryProvider.firebase.terminate)(LoggerProvider.console),
} as const;
