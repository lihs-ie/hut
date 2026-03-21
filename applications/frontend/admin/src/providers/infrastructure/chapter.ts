import { FirebaseChapterRepository } from "@shared/infrastructures/chapter";
import { AdminFirestoreProvider } from "./firestore";

export const AdminChapterRepositoryProvider = {
  firebase: FirebaseChapterRepository(
    AdminFirestoreProvider.firestore.instance,
    AdminFirestoreProvider.firestore.operations,
  ),
} as const;
