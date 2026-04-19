import { validateCriteria } from "@shared/domains/memo";
import { validateSlug } from "@shared/domains/common";
import {
  createMemoFindBySlugWorkflow,
  createMemoSearchWorkflow,
} from "@shared/workflows/memo";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { ReaderMemoRepositoryProvider } from "@/providers/infrastructure/memo";

export const MemoWorkflowProvider = {
  findBySlug: createMemoFindBySlugWorkflow(validateSlug)(
    ReaderMemoRepositoryProvider.firebase.findBySlug,
  )(createPublishedOnlyFilter("Memo"))(LoggerProvider.console),

  search: createMemoSearchWorkflow(validateCriteria)(
    ReaderMemoRepositoryProvider.firebase.search,
  )(LoggerProvider.console),
} as const;
