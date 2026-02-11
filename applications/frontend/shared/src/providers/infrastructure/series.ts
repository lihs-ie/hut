import { FirebaseProvider } from "./firebase";
import { FirebaseSeriesRepository } from "@shared/infrastructures/series";

export const SeriesRepositoryProvider = {
  firebase: FirebaseSeriesRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
} as const;
