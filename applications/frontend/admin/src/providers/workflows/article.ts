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
import { AdminArticleRepositoryProvider } from "../infrastructure/articles";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { validateSlug } from "@shared/domains/common";

export const AdminArticleWorkflowProvider = {
  find: createArticleFindWorkflow(validateArticleIdentifier)(
    AdminArticleRepositoryProvider.firebase.find,
  )(LoggerProvider.console),

  findBySlug: createArticleFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(AdminArticleRepositoryProvider.firebase.findBySlug)(
    createPassthroughFilter(),
  ),

  search: createArticleSearchWorkflow(validateCriteria)(
    AdminArticleRepositoryProvider.firebase.search,
  )(LoggerProvider.console),

  create: createArticleCreateWorkflow(validateArticle)(
    AdminArticleRepositoryProvider.firebase.persist,
  )(LoggerProvider.console),

  edit: createArticleEditWorkflow(validateArticle)(
    AdminArticleRepositoryProvider.firebase.persist,
  )(LoggerProvider.console),

  terminate: createArticleTerminateWorkflow(validateArticleIdentifier)(
    AdminArticleRepositoryProvider.firebase.terminate,
  )(LoggerProvider.console),
} as const;
