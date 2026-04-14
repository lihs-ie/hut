import { createChapterFindBySlugWorkflow } from "@shared/workflows/chapter";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { validateSlug } from "@shared/domains/common";
import { ReaderFirestoreProvider } from "@/providers/infrastructure/firestore";

export const ReaderChapterWorkflowProvider = {
  findBySlug: createChapterFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ReaderFirestoreProvider.chapterRepository.findBySlug)(
    createPublishedOnlyFilter("Chapter"),
  ),
} as const;
