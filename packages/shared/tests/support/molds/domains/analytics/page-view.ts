import {
  DeviceType,
  deviceTypeSchema,
  Referrer,
  referrerSchema,
  PageViewIdentifier,
  pageViewIdentifierSchema,
  PageView,
  pageViewSchema,
  Criteria,
  criteriaSchema,
} from "@shared/domains/analytics/page-view";
import {
  dateKeySchema,
  sessionKeySchema,
} from "@shared/domains/analytics/common";
import type { DateKey, SessionKey, DateRange } from "@shared/domains/analytics/common";
import type { SearchReferenceIdentifier } from "@shared/domains/search-token/reference";
import { EnumMold, Forger, Mold } from "@lihs-ie/forger-ts";
import { DateMold } from "../common/date";
import { SearchReferenceIdentifierMold } from "../search-token/common";

// ---------------------------------------------------------------------------
// DeviceType Mold
// ---------------------------------------------------------------------------

export const DeviceTypeMold = EnumMold({
  DESKTOP: deviceTypeSchema.parse("desktop"),
  MOBILE: deviceTypeSchema.parse("mobile"),
  TABLET: deviceTypeSchema.parse("tablet"),
});

// ---------------------------------------------------------------------------
// Referrer Mold
// ---------------------------------------------------------------------------

export type ReferrerProperties = {
  raw: string | null;
};

export const ReferrerMold = Mold<Referrer, ReferrerProperties>({
  pour: (properties) =>
    referrerSchema.parse({ raw: properties.raw }),
  prepare: (overrides, seed) => ({
    raw:
      overrides.raw !== undefined
        ? overrides.raw
        : seed % 3 === 0
          ? null
          : `https://example-${seed}.com/page`,
  }),
});

// ---------------------------------------------------------------------------
// DateKey Mold
// ---------------------------------------------------------------------------

export type DateKeyProperties = {
  value: string;
};

export const DateKeyMold = Mold<DateKey, DateKeyProperties>({
  pour: (properties) => dateKeySchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ??
      `${2020 + (seed % 5)}-${String((seed % 12) + 1).padStart(2, "0")}-${String((seed % 28) + 1).padStart(2, "0")}`,
  }),
});

// ---------------------------------------------------------------------------
// SessionKey Mold
// ---------------------------------------------------------------------------

export type SessionKeyProperties = {
  value: string;
};

export const SessionKeyMold = Mold<SessionKey, SessionKeyProperties>({
  pour: (properties) => sessionKeySchema.parse(properties.value),
  prepare: (overrides, _seed) => ({
    value: overrides.value ?? crypto.randomUUID(),
  }),
});

// ---------------------------------------------------------------------------
// PageViewIdentifier Mold
// ---------------------------------------------------------------------------

export type PageViewIdentifierProperties = {
  reference: SearchReferenceIdentifier;
  dateKey: DateKey;
  sessionKey: SessionKey;
};

export const PageViewIdentifierMold = Mold<
  PageViewIdentifier,
  PageViewIdentifierProperties
>({
  pour: (properties) =>
    pageViewIdentifierSchema.parse({
      reference: properties.reference,
      dateKey: properties.dateKey,
      sessionKey: properties.sessionKey,
    }),
  prepare: (overrides, seed) => ({
    reference:
      overrides.reference ??
      Forger(SearchReferenceIdentifierMold).forgeWithSeed(seed),
    dateKey: overrides.dateKey ?? Forger(DateKeyMold).forgeWithSeed(seed),
    sessionKey:
      overrides.sessionKey ?? Forger(SessionKeyMold).forgeWithSeed(seed),
  }),
});

// ---------------------------------------------------------------------------
// PageView Mold
// ---------------------------------------------------------------------------

export type PageViewProperties = {
  identifier: PageViewIdentifier;
  referrer: Referrer;
  deviceType: DeviceType;
  createdAt: Date;
};

export const PageViewMold = Mold<PageView, PageViewProperties>({
  pour: (properties) =>
    pageViewSchema.parse({
      identifier: properties.identifier,
      referrer: properties.referrer,
      deviceType: properties.deviceType,
      createdAt: properties.createdAt,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ??
      Forger(PageViewIdentifierMold).forgeWithSeed(seed),
    referrer: overrides.referrer ?? Forger(ReferrerMold).forgeWithSeed(seed),
    deviceType:
      overrides.deviceType ??
      (Forger(DeviceTypeMold).forgeWithSeed(seed) as DeviceType),
    createdAt: overrides.createdAt ?? Forger(DateMold).forgeWithSeed(seed),
  }),
});

// ---------------------------------------------------------------------------
// Criteria Mold
// ---------------------------------------------------------------------------

export type PageViewCriteriaProperties = {
  dateRange: DateRange | null;
  reference: SearchReferenceIdentifier | null;
  deviceType: DeviceType | null;
};

export const PageViewCriteriaMold = Mold<
  Criteria,
  PageViewCriteriaProperties
>({
  pour: (properties) =>
    criteriaSchema.parse({
      dateRange: properties.dateRange,
      reference: properties.reference,
      deviceType: properties.deviceType,
    }),
  prepare: (overrides, _seed) => ({
    dateRange: overrides.dateRange ?? null,
    reference: overrides.reference ?? null,
    deviceType: overrides.deviceType ?? null,
  }),
});
