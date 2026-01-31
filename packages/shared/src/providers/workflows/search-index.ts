import { Logger } from "@shared/aspects/logger";
import { score } from "@shared/aspects/ngram";
import { common } from "@shared/config/common";
import { validateCriteria } from "@shared/domains/search-index";
import { createSearchByIndexWorkflow } from "@shared/workflows/search-index";
import { SearchIndexRepositoryProvider } from "../infrastructure/search-index";
import { ArticleRepositoryProvider } from "../infrastructure/articles";
import { MemoRepositoryProvider } from "../infrastructure/memo";
import { SeriesRepositoryProvider } from "../infrastructure/series";

export const SearchIndexWorkflowProvider = {
  search: createSearchByIndexWorkflow(validateCriteria)(
    Logger(common.ENVIRONMENT)
  )(score(() => 1))(SearchIndexRepositoryProvider.firebase.search)(
    ArticleRepositoryProvider.firebase.ofIdentifiers
  )(MemoRepositoryProvider.firebase.ofIdentifiers)(
    SeriesRepositoryProvider.firebase.ofIdentifiers
  ),
} as const;
