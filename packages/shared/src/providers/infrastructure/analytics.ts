import { FirebasePageViewRepository } from "@shared/infrastructures/analytics/page-view";
import { FirebaseUniqueVisitorRepository } from "@shared/infrastructures/analytics/unique-visitor";
import { FirebaseEngagementRecordRepository } from "@shared/infrastructures/analytics/engagement";
import { FirebaseSearchRecordRepository } from "@shared/infrastructures/analytics/search-record";
import { FirebaseProvider } from "./firebase";

export const AnalyticsRepositoryProvider = {
  pageView: FirebasePageViewRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
  uniqueVisitor: FirebaseUniqueVisitorRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
  engagementRecord: FirebaseEngagementRecordRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
  searchRecord: FirebaseSearchRecordRepository(
    FirebaseProvider.firestore.instance,
    FirebaseProvider.firestore.operations,
  ),
} as const;
