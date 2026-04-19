import { FirebaseSeriesRepository } from "@shared/infrastructures/series";
import { ReaderFirestoreProvider } from "./firebase-select";

export const ReaderSeriesRepositoryProvider = {
  firebase: FirebaseSeriesRepository(
    ReaderFirestoreProvider.instance,
    ReaderFirestoreProvider.operations,
  ),
} as const;
