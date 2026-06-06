import { createChapterFindBySlugWorkflow } from "@shared/workflows/chapter";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { validateSlug } from "@shared/domains/common";
import { ReaderChapterRepositoryProvider } from "@/providers/infrastructure/chapter";

export const ReaderChapterWorkflowProvider = {
  findBySlug: createChapterFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ReaderChapterRepositoryProvider.firebase.findBySlug)(
    createPublishedOnlyFilter("Chapter"),
  ),
} as const;
