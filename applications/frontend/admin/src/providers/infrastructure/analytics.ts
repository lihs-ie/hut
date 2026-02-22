import { FirebasePageViewRepository } from "@shared/infrastructures/analytics/page-view";
import { FirebaseUniqueVisitorRepository } from "@shared/infrastructures/analytics/unique-visitor";
import { FirebaseEngagementRecordRepository } from "@shared/infrastructures/analytics/engagement";
import { FirebaseSearchRecordRepository } from "@shared/infrastructures/analytics/search-record";
import { AdminFirestoreProvider } from "./firestore";

export const AdminAnalyticsRepositoryProvider = {
  pageView: FirebasePageViewRepository(
    AdminFirestoreProvider.firestore.instance,
    AdminFirestoreProvider.firestore.operations,
  ),
  uniqueVisitor: FirebaseUniqueVisitorRepository(
    AdminFirestoreProvider.firestore.instance,
    AdminFirestoreProvider.firestore.operations,
  ),
  engagementRecord: FirebaseEngagementRecordRepository(
    AdminFirestoreProvider.firestore.instance,
    AdminFirestoreProvider.firestore.operations,
  ),
  searchRecord: FirebaseSearchRecordRepository(
    AdminFirestoreProvider.firestore.instance,
    AdminFirestoreProvider.firestore.operations,
  ),
} as const;
