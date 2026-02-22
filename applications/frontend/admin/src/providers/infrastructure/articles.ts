import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import { AdminFirestoreProvider } from "./firestore";

export const AdminArticleRepositoryProvider = {
  firebase: FirebaseArticleRepository(
    AdminFirestoreProvider.firestore.instance,
    AdminFirestoreProvider.firestore.operations,
  ),
} as const;
