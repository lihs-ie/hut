import { createChapterFindBySlugWorkflow } from "@shared/workflows/chapter";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { ChapterRepositoryProvider } from "@shared/providers/infrastructure/chapter";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { validateSlug } from "@shared/domains/common";

export const ReaderChapterWorkflowProvider = {
  findBySlug: createChapterFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ChapterRepositoryProvider.firebase.findBySlug)(
    createPublishedOnlyFilter("Chapter"),
  ),
} as const;
