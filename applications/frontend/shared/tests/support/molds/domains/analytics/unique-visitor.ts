import {
  UniqueVisitorIdentifier,
  uniqueVisitorIdentifierSchema,
  UniqueVisitor,
  uniqueVisitorSchema,
  Criteria,
  criteriaSchema,
} from "@shared/domains/analytics/unique-visitor";
import {
  DateKey,
  dateKeySchema,
  SessionKey,
  sessionKeySchema,
} from "@shared/domains/analytics/common";
import { Forger, Mold } from "@lihs-ie/forger-ts";
import { DateMold } from "../common/date";

const generateSeededUuid = (seed: number): string => {
  const hexFromSeed = (offset: number, length: number): string => {
    const value = Math.abs((seed + offset) * 2654435761);
    return value.toString(16).padStart(length, "0").slice(0, length);
  };

  const timeLow = hexFromSeed(0, 8);
  const timeMid = hexFromSeed(1, 4);
  const timeHighVersion = "4" + hexFromSeed(2, 3);
  const clockSeqVariant =
    ["8", "9", "a", "b"][Math.abs(seed) % 4] + hexFromSeed(3, 3);
  const node = hexFromSeed(4, 12);

  return `${timeLow}-${timeMid}-${timeHighVersion}-${clockSeqVariant}-${node}`;
};

export type DateKeyProperties = { value: string };

export const DateKeyMold = Mold<DateKey, DateKeyProperties>({
  pour: (properties) => dateKeySchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ??
      `${2020 + (seed % 5)}-${String((seed % 12) + 1).padStart(2, "0")}-${String((seed % 28) + 1).padStart(2, "0")}`,
  }),
});

export type SessionKeyProperties = { value: string };

export const SessionKeyMold = Mold<SessionKey, SessionKeyProperties>({
  pour: (properties) => sessionKeySchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? generateSeededUuid(seed),
  }),
});

export type UniqueVisitorIdentifierProperties = {
  dateKey: DateKey;
  sessionKey: SessionKey;
};

export const UniqueVisitorIdentifierMold = Mold<
  UniqueVisitorIdentifier,
  UniqueVisitorIdentifierProperties
>({
  pour: (properties) =>
    uniqueVisitorIdentifierSchema.parse({
      dateKey: properties.dateKey,
      sessionKey: properties.sessionKey,
    }),
  prepare: (overrides, seed) => ({
    dateKey: overrides.dateKey ?? Forger(DateKeyMold).forgeWithSeed(seed),
    sessionKey:
      overrides.sessionKey ?? Forger(SessionKeyMold).forgeWithSeed(seed),
  }),
});

export type UniqueVisitorProperties = {
  identifier: UniqueVisitorIdentifier;
  createdAt: Date;
};

export const UniqueVisitorMold = Mold<UniqueVisitor, UniqueVisitorProperties>({
  pour: (properties) =>
    uniqueVisitorSchema.parse({
      identifier: properties.identifier,
      createdAt: properties.createdAt,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ??
      Forger(UniqueVisitorIdentifierMold).forgeWithSeed(seed),
    createdAt: overrides.createdAt ?? Forger(DateMold).forgeWithSeed(seed),
  }),
});

export type UniqueVisitorCriteriaProperties = {
  dateRange?: { start: Date; end: Date } | null;
};

export const UniqueVisitorCriteriaMold = Mold<
  Criteria,
  UniqueVisitorCriteriaProperties
>({
  pour: (properties) =>
    criteriaSchema.parse({
      dateRange: properties.dateRange,
    }),
  prepare: (overrides) => ({
    dateRange:
      overrides.dateRange === undefined ? null : overrides.dateRange,
  }),
});
