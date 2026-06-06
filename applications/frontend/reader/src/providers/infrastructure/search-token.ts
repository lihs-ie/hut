import { FirebaseSearchTokenRepository } from "@shared/infrastructures/search-token";
import { ReaderFirestoreProvider } from "./firebase-select";

export const ReaderSearchTokenRepositoryProvider = {
  firebase: FirebaseSearchTokenRepository(
    ReaderFirestoreProvider.instance,
    ReaderFirestoreProvider.operations,
  ),
} as const;
