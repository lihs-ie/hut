import {
  createSeriesFindBySlugWorkflow,
  createSeriesSearchWorkflow,
} from "@shared/workflows/series";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { validateSlug } from "@shared/domains/common";
import { validateCriteria } from "@shared/domains/series";
import { ReaderFirestoreProvider } from "@/providers/infrastructure/firestore";

export const ReaderSeriesWorkflowProvider = {
  findBySlug: createSeriesFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ReaderFirestoreProvider.seriesRepository.findBySlug)(
    createPublishedOnlyFilter("Series"),
  ),

  search: createSeriesSearchWorkflow(validateCriteria)(
    ReaderFirestoreProvider.seriesRepository.search,
  )(LoggerProvider.console),
} as const;
