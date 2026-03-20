import { validateCriteria } from "@shared/domains/articles";
import { validateSlug } from "@shared/domains/common";
import {
  createArticleFindBySlugWorkflow,
  createArticleSearchWorkflow,
} from "@shared/workflows/article";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { ArticleRepositoryProvider } from "@shared/providers/infrastructure/articles";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";

export const ArticleWorkflowProvider = {
  findBySlug: createArticleFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ArticleRepositoryProvider.firebase.findBySlug)(
    createPublishedOnlyFilter("Article"),
  ),

  search: createArticleSearchWorkflow(validateCriteria)(
    ArticleRepositoryProvider.firebase.search,
  )(LoggerProvider.console),
} as const;
