import { ulid } from "ulid";
import {
  SearchRecord,
  SearchRecordIdentifier,
  searchRecordIdentifierSchema,
  searchRecordSchema,
  SearchKeyword,
  searchKeywordSchema,
  Criteria,
  criteriaSchema,
} from "@shared/domains/analytics/search-record";
import { DateKey, dateKeySchema } from "@shared/domains/analytics/common";
import { Forger, Mold, StringMold } from "@lihs-ie/forger-ts";
import { TagIdentifier, tagIdentifierSchema } from "@shared/domains/attributes/tag";
import { ContentType, contentTypeSchema } from "@shared/domains/search-token/reference";

export type SearchRecordIdentifierProperties = {
  value: string;
};

export const SearchRecordIdentifierMold = Mold<
  SearchRecordIdentifier,
  SearchRecordIdentifierProperties
>({
  pour: (properties) => searchRecordIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? ulid(seed % 1000000),
  }),
});

export type SearchKeywordProperties = {
  value: string;
};

export const SearchKeywordMold = Mold<SearchKeyword, SearchKeywordProperties>({
  pour: (properties) => searchKeywordSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? Forger(StringMold(1, 50)).forgeWithSeed(seed),
  }),
});

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

export type TagIdentifierProperties = {
  value: string;
};

export const TagIdentifierForAnalyticsMold = Mold<
  TagIdentifier,
  TagIdentifierProperties
>({
  pour: (properties) => tagIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? ulid(seed % 1000000),
  }),
});

const CONTENT_TYPES = ["article", "memo", "series"] as const;

export type SearchRecordProperties = {
  identifier: SearchRecordIdentifier;
  dateKey: DateKey;
  keyword: SearchKeyword;
  resultCount: number;
  tags: TagIdentifier[] | null;
  contentType: ContentType | null;
  createdAt: Date;
};

export const SearchRecordMold = Mold<SearchRecord, SearchRecordProperties>({
  pour: (properties) =>
    searchRecordSchema.parse({
      identifier: properties.identifier,
      dateKey: properties.dateKey,
      keyword: properties.keyword,
      resultCount: properties.resultCount,
      tags: properties.tags,
      contentType: properties.contentType,
      createdAt: properties.createdAt,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ??
      Forger(SearchRecordIdentifierMold).forgeWithSeed(seed),
    dateKey: overrides.dateKey ?? Forger(DateKeyMold).forgeWithSeed(seed),
    keyword: overrides.keyword ?? Forger(SearchKeywordMold).forgeWithSeed(seed),
    resultCount: overrides.resultCount ?? Math.abs(seed % 100),
    tags:
      overrides.tags ??
      (seed % 3 === 0
        ? null
        : [Forger(TagIdentifierForAnalyticsMold).forgeWithSeed(seed)]),
    contentType:
      overrides.contentType ??
      (seed % 4 === 0
        ? null
        : contentTypeSchema.parse(CONTENT_TYPES[seed % 3])),
    createdAt:
      overrides.createdAt ??
      new Date(Date.UTC(2020 + (seed % 5), seed % 12, (seed % 28) + 1)),
  }),
});

export type CriteriaProperties = {
  dateRange?: { start: Date; end: Date } | null;
  keyword?: SearchKeyword | null;
  hasResults?: boolean | null;
};

export const CriteriaMold = Mold<Criteria, CriteriaProperties>({
  pour: (properties) =>
    criteriaSchema.parse({
      dateRange: properties.dateRange,
      keyword: properties.keyword,
      hasResults: properties.hasResults,
    }),
  prepare: (overrides, seed) => ({
    dateRange:
      overrides.dateRange === undefined
        ? seed % 2 === 0
          ? {
              start: new Date(Date.UTC(2020, 0, 1)),
              end: new Date(Date.UTC(2025, 0, 1)),
            }
          : null
        : overrides.dateRange,
    keyword:
      overrides.keyword === undefined
        ? seed % 2 === 0
          ? Forger(SearchKeywordMold).forgeWithSeed(seed)
          : null
        : overrides.keyword,
    hasResults:
      overrides.hasResults === undefined
        ? seed % 3 === 0
          ? true
          : null
        : overrides.hasResults,
  }),
});
