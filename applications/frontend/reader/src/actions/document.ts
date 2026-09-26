"use server";

import { cache } from "react";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { PrivacyPolicy } from "@shared/domains/document";
import { FirebaseSiteDocumentRepository } from "@shared/infrastructures/documents";
import { createGetPrivacyPolicyWorkflow } from "@shared/workflows/document";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { ReaderFirestoreProvider } from "@/providers/infrastructure/firebase-select";

/** Reads the public privacy policy through the Reader's Firestore adapter. */
export const getPrivacyPolicy = cache(async (): Promise<PrivacyPolicy> => {
  const repository = FirebaseSiteDocumentRepository(
    ReaderFirestoreProvider.instance,
    ReaderFirestoreProvider.operations,
  );
  const find = createGetPrivacyPolicyWorkflow(repository.find)(LoggerProvider.console);
  return unwrapForNextJs(find({ payload: null, now: new Date() }));
});
