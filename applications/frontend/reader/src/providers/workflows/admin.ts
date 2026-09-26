import { createAdminFindWorkflow } from "@shared/workflows/admin";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { ReaderAdminRepositoryProvider } from "@/providers/infrastructure/admin";

export const ReaderAdminWorkflowProvider = {
  find: createAdminFindWorkflow(LoggerProvider.console)(
    ReaderAdminRepositoryProvider.firebase.find,
  ),
} as const;
