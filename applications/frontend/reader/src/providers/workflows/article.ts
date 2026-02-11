import { validateSlug } from "@shared/domains/common";
import { createArticleFindBySlugWorkflow } from "@shared/workflows/article";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { ArticleRepositoryProvider } from "@shared/providers/infrastructure/articles";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";

export const ArticleWorkflowProvider = {
  findBySlug: createArticleFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ArticleRepositoryProvider.firebase.findBySlug)(
    createPublishedOnlyFilter("Article"),
  ),
} as const;
