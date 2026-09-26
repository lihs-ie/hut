import { validateCriteria } from "@shared/domains/search-token";
import { createSearchByTokenWorkflow } from "@shared/workflows/search-token";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { ReaderSearchTokenRepositoryProvider } from "@/providers/infrastructure/search-token";
import { ReaderArticleRepositoryProvider } from "@/providers/infrastructure/articles";
import { ReaderMemoRepositoryProvider } from "@/providers/infrastructure/memo";
import { ReaderSeriesRepositoryProvider } from "@/providers/infrastructure/series";

export const ReaderSearchTokenWorkflowProvider = {
  search: createSearchByTokenWorkflow(validateCriteria)(LoggerProvider.console)(
    ReaderSearchTokenRepositoryProvider.firebase.ofIdentifiers,
  )(ReaderArticleRepositoryProvider.firebase.ofIdentifiers)(
    ReaderMemoRepositoryProvider.firebase.ofIdentifiers,
  )(ReaderSeriesRepositoryProvider.firebase.ofIdentifiers),
} as const;
