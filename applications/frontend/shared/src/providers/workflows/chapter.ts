import { createChapterFindBySlugWorkflow } from "@shared/workflows/chapter";
import { createPassthroughFilter } from "@shared/workflows/common";
import { ChapterRepositoryProvider } from "../infrastructure/chapter";
import { LoggerProvider } from "../infrastructure/logger";
import { validateSlug } from "@shared/domains/common";

export const ChapterWorkflowProvider = {
  findBySlug: createChapterFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ChapterRepositoryProvider.firebase.findBySlug)(
    createPassthroughFilter(),
  ),
} as const;
