import { createSeriesFindBySlugWorkflow } from "@shared/workflows/series";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { validateSlug } from "@shared/domains/common";
import { validateCriteria } from "@shared/domains/series";
import { createSeriesSearchWorkflow } from "@shared/workflows/series";
import { ReaderSeriesRepositoryProvider } from "@/providers/infrastructure/series";

export const ReaderSeriesWorkflowProvider = {
  findBySlug: createSeriesFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ReaderSeriesRepositoryProvider.firebase.findBySlug)(
    createPublishedOnlyFilter("Series"),
  ),

  search: createSeriesSearchWorkflow(validateCriteria)(
    ReaderSeriesRepositoryProvider.firebase.search,
  )(LoggerProvider.console),
} as const;
