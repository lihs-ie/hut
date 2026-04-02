import { createChapterFindBySlugWorkflow } from "@shared/workflows/chapter";
import { ChapterRepositoryProvider } from "../infrastructure/chapter";
import { LoggerProvider } from "../infrastructure/logger";
import { validateSlug } from "@shared/domains/common";

export const ChapterWorkflowProvider = {
  findBySlug: createChapterFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console
  )(ChapterRepositoryProvider.firebase.findBySlug),
} as const;
