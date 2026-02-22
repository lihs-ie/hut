import {
  createGetPrivacyPolicyWorkflow,
  createPrivacyPolicyPersistWorkflow,
} from "@shared/workflows/document";
import { AdminDocumentRepositoryProvider } from "../infrastructure/document";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";

export const AdminDocumentWorkflowProvider = {
  GetPrivacyPolicy: createGetPrivacyPolicyWorkflow(
    AdminDocumentRepositoryProvider.firebase.find,
  )(LoggerProvider.console),

  PersistPrivatePolicy: createPrivacyPolicyPersistWorkflow(
    AdminDocumentRepositoryProvider.firebase.find,
  )(AdminDocumentRepositoryProvider.firebase.persist)(LoggerProvider.console),
} as const;
