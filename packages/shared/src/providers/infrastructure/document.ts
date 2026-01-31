import { FirebaseSiteDocumentRepository } from "@shared/infrastructures/documents";
import { FirebaseProvider } from "./firebase";

export const DocumentRepositoryProvider = {
  firebase: FirebaseSiteDocumentRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
} as const;
