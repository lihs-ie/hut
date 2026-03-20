import { validateCriteria } from "@shared/domains/memo";
import { validateSlug } from "@shared/domains/common";
import {
  createMemoFindBySlugWorkflow,
  createMemoSearchWorkflow,
} from "@shared/workflows/memo";
import { createPublishedOnlyFilter } from "@shared/workflows/common";
import { MemoRepositoryProvider } from "@shared/providers/infrastructure/memo";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";

export const MemoWorkflowProvider = {
  findBySlug: createMemoFindBySlugWorkflow(validateSlug)(
    MemoRepositoryProvider.firebase.findBySlug,
  )(createPublishedOnlyFilter("Memo"))(LoggerProvider.console),

  search: createMemoSearchWorkflow(validateCriteria)(
    MemoRepositoryProvider.firebase.search,
  )(LoggerProvider.console),
} as const;
