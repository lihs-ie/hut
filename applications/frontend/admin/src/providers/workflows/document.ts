import {
  createGetPrivacyPolicyWorkflow,
  createPrivacyPolicyPersistWorkflow,
} from "@shared/workflows/document";
import { DocumentRepositoryProvider } from "@shared/providers/infrastructure/document";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";

export const AdminDocumentWorkflowProvider = {
  GetPrivacyPolicy: createGetPrivacyPolicyWorkflow(
    DocumentRepositoryProvider.firebase.find,
  )(LoggerProvider.console),

  PersistPrivatePolicy: createPrivacyPolicyPersistWorkflow(
    DocumentRepositoryProvider.firebase.find,
  )(DocumentRepositoryProvider.firebase.persist)(LoggerProvider.console),
} as const;
