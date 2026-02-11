import { FirebaseArticleRepository } from "@shared/infrastructures/articles";
import { FirebaseProvider } from "./firebase";

export const ArticleRepositoryProvider = {
  firebase: FirebaseArticleRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
} as const;
