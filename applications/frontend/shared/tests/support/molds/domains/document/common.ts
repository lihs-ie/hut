import {
  PrivacyPolicy,
  PrivacyPolicySection,
  privacyPolicySchema,
  privacyPolicySection,
  SiteDocument,
  siteDocumentSchema,
} from "@shared/domains/document";
import { Timeline } from "@shared/domains/common";
import { Forger, Mold, StringMold } from "@lihs-ie/forger-ts";
import { TimelineMold } from "../common/date";

export type PrivacyPolicySectionProperties = {
  headline: string;
  body: string;
  list: string[] | null;
};

export const PrivacyPolicySectionMold = Mold<
  PrivacyPolicySection,
  PrivacyPolicySectionProperties
>({
  pour: (properties) =>
    privacyPolicySection.parse({
      headline: properties.headline,
      body: properties.body,
      list: properties.list,
    }),
  prepare: (overrides, seed) => ({
    headline:
      overrides.headline ?? Forger(StringMold(1, 200)).forgeWithSeed(seed),
    body: overrides.body ?? Forger(StringMold(1, 1000)).forgeWithSeed(seed),
    list:
      overrides.list !== undefined
        ? overrides.list
        : seed % 2 === 0
          ? Forger(StringMold(1, 100)).forgeMultiWithSeed(3, seed)
          : null,
  }),
});

export type PrivacyPolicyProperties = {
  sections: PrivacyPolicySection[];
  timeline: Timeline;
};

export const PrivacyPolicyMold = Mold<PrivacyPolicy, PrivacyPolicyProperties>({
  pour: (properties) =>
    privacyPolicySchema.parse({
      sections: properties.sections,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => ({
    sections:
      overrides.sections ??
      Forger(PrivacyPolicySectionMold).forgeMultiWithSeed(3, seed),
    timeline: overrides.timeline ?? Forger(TimelineMold).forgeWithSeed(seed),
  }),
});

export type SiteDocumentProperties = {
  privacy: PrivacyPolicy;
};

export const SiteDocumentMold = Mold<SiteDocument, SiteDocumentProperties>({
  pour: (properties) =>
    siteDocumentSchema.parse({
      privacy: properties.privacy,
    }),
  prepare: (overrides, seed) => ({
    privacy:
      overrides.privacy ?? Forger(PrivacyPolicyMold).forgeWithSeed(seed),
  }),
});

