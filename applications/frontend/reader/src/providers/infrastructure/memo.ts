import { FirebaseMemoRepository } from "@shared/infrastructures/memos";
import { ReaderFirestoreProvider } from "./firebase-select";

export const ReaderMemoRepositoryProvider = {
  firebase: FirebaseMemoRepository(
    ReaderFirestoreProvider.instance,
    ReaderFirestoreProvider.operations,
  ),
} as const;
