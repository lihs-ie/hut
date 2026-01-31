import { FirebaseMemoRepository } from "@shared/infrastructures/memos";
import { FirebaseProvider } from "./firebase";

export const MemoRepositoryProvider = {
  firebase: FirebaseMemoRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
} as const;
