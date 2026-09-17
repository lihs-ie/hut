import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import { ReaderFirestoreProvider } from "./firebase-select";

export const ReaderArticleRepositoryProvider = {
  firebase: FirebaseArticleRepository(
    ReaderFirestoreProvider.instance,
    ReaderFirestoreProvider.operations,
  ),
} as const;
