import { FirebaseTagRepository } from "@shared/infrastructures/tags";
import { AdminFirestoreProvider } from "./firestore";

export const AdminTagRepositoryProvider = {
  firebase: FirebaseTagRepository(
    AdminFirestoreProvider.firestore.instance,
    AdminFirestoreProvider.firestore.operations,
  ),
} as const;
