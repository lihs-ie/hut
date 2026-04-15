import { validateCriteria } from "@shared/domains/articles";
import { validateSlug } from "@shared/domains/common";
import {
  createArticleFindBySlugWorkflow,
  createArticleSearchWorkflow,
} from "@shared/workflows/article";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { ReaderFirestoreProvider } from "@/providers/infrastructure/firestore";

export const ArticleWorkflowProvider = {
  findBySlug: createArticleFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ReaderFirestoreProvider.articleRepository.findBySlug)(
    createPublishedOnlyFilter("Article"),
  ),

  search: createArticleSearchWorkflow(validateCriteria)(
    ReaderFirestoreProvider.articleRepository.search,
  )(LoggerProvider.console),
} as const;
