import { validateChapter } from "@shared/domains/series/chapter";
import { validateSlug } from "@shared/domains/common";
import {
  createChapterPersistWorkflow,
  createChapterFindBySlugWorkflow,
} from "@shared/workflows/chapter";
import { AdminChapterRepositoryProvider } from "../infrastructure/chapter";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";

export const AdminChapterWorkflowProvider = {
  persist: createChapterPersistWorkflow(validateChapter)(
    AdminChapterRepositoryProvider.firebase.persist,
  )(LoggerProvider.console),

  findBySlug: createChapterFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(AdminChapterRepositoryProvider.firebase.findBySlug),
} as const;
