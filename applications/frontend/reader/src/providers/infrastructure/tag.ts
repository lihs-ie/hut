import { FirebaseTagRepository } from "@shared/infrastructures/tags";
import { ReaderFirestoreProvider } from "./firebase-select";

export const ReaderTagRepositoryProvider = {
  firebase: FirebaseTagRepository(
    ReaderFirestoreProvider.instance,
    ReaderFirestoreProvider.operations,
  ),
} as const;
