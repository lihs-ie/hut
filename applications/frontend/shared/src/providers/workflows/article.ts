import {
  validateArticle,
  validateArticleIdentifier,
  validateCriteria,
} from "@shared/domains/articles";
import {
  createArticleCreateWorkflow,
  createArticleEditWorkflow,
  createArticleFindBySlugWorkflow,
  createArticleFindWorkflow,
  createArticleSearchWorkflow,
  createArticleTerminateWorkflow,
} from "@shared/workflows/article";
import { createPassthroughFilter } from "@shared/workflows/common";
import { ArticleRepositoryProvider } from "../infrastructure/articles";
import { LoggerProvider } from "../infrastructure/logger";
import { validateSlug } from "@shared/domains/common";

export const ArticleWorkflowProvider = {
  find: createArticleFindWorkflow(validateArticleIdentifier)(
    ArticleRepositoryProvider.firebase.find,
  )(LoggerProvider.console),

  findBySlug: createArticleFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ArticleRepositoryProvider.firebase.findBySlug)(
    createPassthroughFilter(),
  ),

  search: createArticleSearchWorkflow(validateCriteria)(
    ArticleRepositoryProvider.firebase.search,
  )(LoggerProvider.console),

  create: createArticleCreateWorkflow(validateArticle)(
    ArticleRepositoryProvider.firebase.persist,
  )(LoggerProvider.console),

  edit: createArticleEditWorkflow(validateArticle)(
    ArticleRepositoryProvider.firebase.persist,
  )(LoggerProvider.console),

  terminate: createArticleTerminateWorkflow(validateArticleIdentifier)(
    ArticleRepositoryProvider.firebase.terminate,
  )(LoggerProvider.console),
} as const;
