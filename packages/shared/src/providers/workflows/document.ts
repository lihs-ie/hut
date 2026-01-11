import {
  createGetPrivacyPolicyWorkflow,
  createPrivacyPolicyPersistWorkflow,
} from "@shared/workflows/document";
import { DocumentRepositoryProvider } from "../infrastructure/document";
import { LoggerProvider } from "../infrastructure/logger";

export const DocumentWorkflowProvider = {
  GetPrivacyPolicy: createGetPrivacyPolicyWorkflow(
    DocumentRepositoryProvider.firebase.find,
  )(LoggerProvider.console),

  PersistPrivatePolicy: createPrivacyPolicyPersistWorkflow(
    DocumentRepositoryProvider.firebase.find,
  )(DocumentRepositoryProvider.firebase.persist)(LoggerProvider.console),
} as const;
