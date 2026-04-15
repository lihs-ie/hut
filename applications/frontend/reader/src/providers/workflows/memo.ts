import { validateCriteria } from "@shared/domains/memo";
import { validateSlug } from "@shared/domains/common";
import {
  createMemoFindBySlugWorkflow,
  createMemoSearchWorkflow,
} from "@shared/workflows/memo";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { ReaderFirestoreProvider } from "@/providers/infrastructure/firestore";

export const MemoWorkflowProvider = {
  findBySlug: createMemoFindBySlugWorkflow(validateSlug)(
    ReaderFirestoreProvider.memoRepository.findBySlug,
  )(createPublishedOnlyFilter("Memo"))(LoggerProvider.console),

  search: createMemoSearchWorkflow(validateCriteria)(
    ReaderFirestoreProvider.memoRepository.search,
  )(LoggerProvider.console),
} as const;
