import { validateCriteria } from "@shared/domains/search-token";
import { createSearchByTokenWorkflow } from "@shared/workflows/search-token";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { ReaderSearchTokenRepositoryProvider } from "@/providers/infrastructure/search-token";
import { ReaderMemoRepositoryProvider } from "@/providers/infrastructure/memo";
import { ReaderSeriesRepositoryProvider } from "@/providers/infrastructure/series";
import { ok } from "@shared/aspects/result";

export const ReaderSearchTokenWorkflowProvider = {
  search: createSearchByTokenWorkflow(validateCriteria)(LoggerProvider.console)(
    ReaderSearchTokenRepositoryProvider.firebase.ofIdentifiers,
  )((_identifiers) => ok([]).toAsync())(
    ReaderMemoRepositoryProvider.firebase.ofIdentifiers,
  )(ReaderSeriesRepositoryProvider.firebase.ofIdentifiers),
} as const;
