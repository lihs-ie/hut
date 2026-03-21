import { FirebaseChapterRepository } from "@shared/infrastructures/chapter";
import { FirebaseProvider } from "./firebase";

export const ChapterRepositoryProvider = {
  firebase: FirebaseChapterRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
} as const;
