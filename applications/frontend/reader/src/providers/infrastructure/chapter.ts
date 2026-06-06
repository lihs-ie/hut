import { FirebaseChapterRepository } from "@shared/infrastructures/chapter";
import { ReaderFirestoreProvider } from "./firebase-select";

export const ReaderChapterRepositoryProvider = {
  firebase: FirebaseChapterRepository(
    ReaderFirestoreProvider.instance,
    ReaderFirestoreProvider.operations,
  ),
} as const;
