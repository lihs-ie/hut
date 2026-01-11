import { validateCriteria } from "@shared/domains/search-token";
import { createSearchByTokenWorkflow } from "@shared/workflows/search-token";
import { SearchTokenRepositoryProvider } from "../infrastructure/search-token";
import { ArticleRepositoryProvider } from "../infrastructure/articles";
import { MemoRepositoryProvider } from "../infrastructure/memo";
import { SeriesRepositoryProvider } from "../infrastructure/series";
import { LoggerProvider } from "../infrastructure/logger";

export const SearchTokenWorkflowProvider = {
  search: createSearchByTokenWorkflow(validateCriteria)(LoggerProvider.console)(
    SearchTokenRepositoryProvider.firebase.ofIdentifiers,
  )(ArticleRepositoryProvider.firebase.ofIdentifiers)(
    MemoRepositoryProvider.firebase.ofIdentifiers,
  )(SeriesRepositoryProvider.firebase.ofIdentifiers),
} as const;
