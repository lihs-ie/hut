import { validateCriteria } from "@shared/domains/articles";
import { validateSlug } from "@shared/domains/common";
import {
  createArticleFindBySlugWorkflow,
  createArticleSearchWorkflow,
} from "@shared/workflows/article";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { ReaderArticleRepositoryProvider } from "@/providers/infrastructure/articles";

export const ArticleWorkflowProvider = {
  findBySlug: createArticleFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ReaderArticleRepositoryProvider.firebase.findBySlug)(
    createPublishedOnlyFilter("Article"),
  ),

  search: createArticleSearchWorkflow(validateCriteria)(
    ReaderArticleRepositoryProvider.firebase.search,
  )(LoggerProvider.console),
} as const;
