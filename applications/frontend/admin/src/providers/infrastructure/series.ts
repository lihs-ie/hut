import { FirebaseSeriesRepository } from "@shared/infrastructures/series";
import { AdminFirestoreProvider } from "./firestore";

export const AdminSeriesRepositoryProvider = {
  firebase: FirebaseSeriesRepository(
    AdminFirestoreProvider.firestore.instance,
    AdminFirestoreProvider.firestore.operations,
  ),
} as const;
