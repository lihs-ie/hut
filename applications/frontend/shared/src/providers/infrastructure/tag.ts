import { FirebaseTagRepository } from "@shared/infrastructures/tags";
import { FirebaseProvider } from "./firebase";

export const TagRepositoryProvider = {
  firebase: FirebaseTagRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
} as const;
