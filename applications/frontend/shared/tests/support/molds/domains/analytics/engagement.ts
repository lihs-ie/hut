import {
  DwellTime,
  dwellTimeSchema,
  ScrollDepth,
  scrollDepthSchema,
  EngagementRecordIdentifier,
  engagementRecordIdentifierSchema,
  EngagementRecord,
  engagementRecordSchema,
} from "@shared/domains/analytics/engagement";
import {
  DateKey,
  dateKeySchema,
  SessionKey,
  sessionKeySchema,
} from "@shared/domains/analytics/common";
import { SearchReferenceIdentifier } from "@shared/domains/search-token";
import { Forger, Mold } from "@lihs-ie/forger-ts";
import { SearchReferenceIdentifierMold } from "../search-token/common";
import { DateMold } from "../common/date";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// DwellTime Mold
// ---------------------------------------------------------------------------

export type DwellTimeProperties = {
  value: number;
};

export const DwellTimeMold = Mold<DwellTime, DwellTimeProperties>({
  pour: (properties) => dwellTimeSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Math.abs(seed % 86_401),
  }),
});

// ---------------------------------------------------------------------------
// ScrollDepth Mold
// ---------------------------------------------------------------------------

export type ScrollDepthProperties = {
  value: number;
};

export const ScrollDepthMold = Mold<ScrollDepth, ScrollDepthProperties>({
  pour: (properties) => scrollDepthSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Math.abs(seed % 101),
  }),
});

// ---------------------------------------------------------------------------
// EngagementRecordIdentifier Mold
// ---------------------------------------------------------------------------

export type EngagementRecordIdentifierProperties = {
  reference: SearchReferenceIdentifier;
  dateKey: DateKey;
  sessionKey: SessionKey;
};

export const EngagementRecordIdentifierMold = Mold<
  EngagementRecordIdentifier,
  EngagementRecordIdentifierProperties
>({
  pour: (properties) =>
    engagementRecordIdentifierSchema.parse({
      reference: properties.reference,
      dateKey: properties.dateKey,
      sessionKey: properties.sessionKey,
    }),
  prepare: (overrides, seed) => {
    const date = Forger(DateMold).forgeWithSeed(seed);
    const year = date.getFullYear();
    const month = String(date.getMonth() + 1).padStart(2, "0");
    const day = String(date.getDate()).padStart(2, "0");
    const dateKeyValue = `${year}-${month}-${day}`;

    return {
      reference:
        overrides.reference ??
        Forger(SearchReferenceIdentifierMold).forgeWithSeed(seed),
      dateKey: overrides.dateKey ?? dateKeySchema.parse(dateKeyValue),
      sessionKey:
        overrides.sessionKey ??
        sessionKeySchema.parse(
          generateSeededUuid(seed),
        ),
    };
  },
});

// ---------------------------------------------------------------------------
// EngagementRecord Mold
// ---------------------------------------------------------------------------

export type EngagementRecordProperties = {
  identifier: EngagementRecordIdentifier;
  dwellTime: DwellTime;
  scrollDepth: ScrollDepth;
  createdAt: Date;
  updatedAt: Date;
};

export const EngagementRecordMold = Mold<
  EngagementRecord,
  EngagementRecordProperties
>({
  pour: (properties) =>
    engagementRecordSchema.parse({
      identifier: properties.identifier,
      dwellTime: properties.dwellTime,
      scrollDepth: properties.scrollDepth,
      createdAt: properties.createdAt,
      updatedAt: properties.updatedAt,
    }),
  prepare: (overrides, seed) => {
    const createdAt =
      overrides.createdAt ?? Forger(DateMold).forgeWithSeed(seed);
    const updatedAt =
      overrides.updatedAt ??
      new Date(
        createdAt.getTime() + Math.abs(seed % 5) * 24 * 60 * 60 * 1000,
      );

    return {
      identifier:
        overrides.identifier ??
        Forger(EngagementRecordIdentifierMold).forgeWithSeed(seed),
      dwellTime:
        overrides.dwellTime ?? Forger(DwellTimeMold).forgeWithSeed(seed),
      scrollDepth:
        overrides.scrollDepth ?? Forger(ScrollDepthMold).forgeWithSeed(seed),
      createdAt,
      updatedAt,
    };
  },
});
