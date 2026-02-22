import { FirebaseSiteDocumentRepository } from "@shared/infrastructures/documents";
import { AdminFirestoreProvider } from "./firestore";

export const AdminDocumentRepositoryProvider = {
  firebase: FirebaseSiteDocumentRepository(
    AdminFirestoreProvider.firestore.instance,
    AdminFirestoreProvider.firestore.operations,
  ),
} as const;
