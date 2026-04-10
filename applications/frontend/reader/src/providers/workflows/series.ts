import { createSeriesFindBySlugWorkflow } from "@shared/workflows/series";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { SeriesRepositoryProvider } from "@shared/providers/infrastructure/series";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { validateSlug } from "@shared/domains/common";

export const ReaderSeriesWorkflowProvider = {
  findBySlug: createSeriesFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(SeriesRepositoryProvider.firebase.findBySlug)(
    createPublishedOnlyFilter("Series"),
  ),
} as const;
