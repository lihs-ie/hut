import { FirebaseSiteDocumentRepository } from "@shared/infrastructures/documents";
import { ReaderFirestoreProvider } from "./firebase-select";

export const ReaderDocumentRepositoryProvider = {
  firebase: FirebaseSiteDocumentRepository(
    ReaderFirestoreProvider.instance,
    ReaderFirestoreProvider.operations,
  ),
} as const;
