import { createGetPrivacyPolicyWorkflow } from "@shared/workflows/document";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { ReaderDocumentRepositoryProvider } from "@/providers/infrastructure/document";

export const ReaderDocumentWorkflowProvider = {
  GetPrivacyPolicy: createGetPrivacyPolicyWorkflow(
    ReaderDocumentRepositoryProvider.firebase.find,
  )(LoggerProvider.console),
} as const;
