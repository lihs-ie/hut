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
  createSeriesTerminateWorkflow,
} from "@shared/workflows/series";
import { SeriesRepositoryProvider } from "../infrastructure/series";
import { LoggerProvider } from "../infrastructure/logger";
import { validateSlug } from "@shared/domains/common";

export const SeriesWorkflowProvider = {
  find: createSeriesFindWorkflow(validateSeriesIdentifier)(
    SeriesRepositoryProvider.firebase.find
  )(LoggerProvider.console),

  findBySlug: createSeriesFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console
  )(SeriesRepositoryProvider.firebase.findBySlug),

  search: createSeriesSearchWorkflow(validateCriteria)(
    SeriesRepositoryProvider.firebase.search
  )(LoggerProvider.console),

  persist: createSeriesPersistWorkflow(validateSeries)(
    SeriesRepositoryProvider.firebase.persist
  )(LoggerProvider.console),

  terminate: createSeriesTerminateWorkflow(validateSeriesIdentifier)(
    SeriesRepositoryProvider.firebase.terminate
  )(LoggerProvider.console),
} as const;
