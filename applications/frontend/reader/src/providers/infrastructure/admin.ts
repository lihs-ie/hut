import { FirebaseAdminRepository } from "@shared/infrastructures/admin";
import { ReaderFirestoreProvider } from "./firebase-select";

export const ReaderAdminRepositoryProvider = {
  firebase: FirebaseAdminRepository(
    ReaderFirestoreProvider.instance,
    ReaderFirestoreProvider.operations,
  ),
} as const;
