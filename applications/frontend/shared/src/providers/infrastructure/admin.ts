import { FirebaseAdminRepository } from "@shared/infrastructures/admin";
import { FirebaseProvider } from "./firebase";

export const AdminRepositoryProvider = {
  firebase: FirebaseAdminRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
} as const;
