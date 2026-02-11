import { FirebaseProvider } from "./firebase";
import { FirebaseSearchIndexRepository } from "@shared/infrastructures/search-index";

export const SearchIndexRepositoryProvider = {
  firebase: FirebaseSearchIndexRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
} as const;
