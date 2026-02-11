import { generateDateKeys } from "@shared/domains/analytics/common";
import type { DateRange } from "@shared/domains/analytics/common";

export const CONTENT_TYPES = ["article", "memo", "series"] as const;

export const resolveDateKeysFromCriteria = (criteria: {
  dateRange?: DateRange | null;
}): Set<string> | null => {
  if (!criteria.dateRange) return null;
  return new Set(generateDateKeys(criteria.dateRange));
};

export const toCompositeDocumentId = (identifier: {
  dateKey: string;
  sessionKey: string;
}): string => `${identifier.dateKey}:${identifier.sessionKey}`;

export const parseCompositeDocumentId = (
  compositeId: string,
): { dateKey: string; sessionKey: string } => {
  const [dateKey, ...sessionParts] = compositeId.split(":");
  return {
    dateKey: dateKey ?? "",
    sessionKey: sessionParts.join(":"),
  };
};

export const extractCountFromSnapshot = (
  snapshotData: Record<string, unknown> | undefined,
): number => {
  if (snapshotData && typeof snapshotData.count === "number") {
    return snapshotData.count;
  }
  return 0;
};
