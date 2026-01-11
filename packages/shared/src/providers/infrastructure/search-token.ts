import { FirebaseProvider } from "./firebase";
import { FirebaseSearchTokenRepository } from "@shared/infrastructures/search-token";

export const SearchTokenRepositoryProvider = {
  firebase: FirebaseSearchTokenRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations
  ),
} as const;
