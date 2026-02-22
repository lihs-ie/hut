import { FirebaseMemoRepository } from "@shared/infrastructures/memos";
import { AdminFirestoreProvider } from "./firestore";

export const AdminMemoRepositoryProvider = {
  firebase: FirebaseMemoRepository(
    AdminFirestoreProvider.firestore.instance,
    AdminFirestoreProvider.firestore.operations,
  ),
} as const;
