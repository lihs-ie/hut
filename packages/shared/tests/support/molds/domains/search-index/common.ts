import {
  ContentType,
  contentTypeSchema,
  Criteria,
  criteriaSchema,
  Order,
  orderSchema,
  SearchIndex,
  SearchIndexExcerpt,
  SearchIndexIdentifier,
  searchIndexIdentifierSchema,
  SearchIndexRepository,
  SearchIndexTitle,
  Sort,
  titleSchema,
  excerptSchema,
  searchIndexSchema,
} from "@shared/domains/search-index/common";
import { ulid } from "ulid";
import { DateMold, TimelineMold } from "../common/date";
import { Timeline } from "@shared/domains/common";
import {
  ArticleIdentifier,
  articleIdentifierSchema,
} from "@shared/domains/articles";
import { MemoIdentifier, memoIdentifierSchema } from "@shared/domains/memo";
import {
  SeriesIdentifier,
  seriesIdentifierSchema,
} from "@shared/domains/series";
import { fromPromise } from "@shared/aspects/result";
import { unexpectedError, UnexpectedError } from "@shared/aspects/error";
import { EnumMold, Forger, Mold, StringMold } from "@lihs-ie/forger-ts";
import { TagIdentifierMold } from "../attributes/tag";
import { TagIdentifier } from "@shared/domains/attributes/tag";

export const ContentTypeMold = EnumMold(ContentType);

export const SortMold = EnumMold(Sort);

export const OrderMold = EnumMold(Order);

export type SearchIndexIdentifierProperties = {
  value: string;
};

export const SearchIndexIdentifierMold = Mold<
  SearchIndexIdentifier,
  SearchIndexIdentifierProperties
>({
  pour: (properties) => searchIndexIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Forger(DateMold).forgeWithSeed(seed).getTime()),
  }),
});

export type SearchIndexTitleProperties = {
  value: string;
};

export const SearchIndexTitleMold = Mold<
  SearchIndexTitle,
  SearchIndexTitleProperties
>({
  pour: (properties) => titleSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Forger(StringMold(1, 100)).forgeWithSeed(seed),
  }),
});

export type SearchIndexExcerptProperties = {
  value: string;
};

export const SearchIndexExcerptMold = Mold<
  SearchIndexExcerpt,
  SearchIndexExcerptProperties
>({
  pour: (properties) => excerptSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Forger(StringMold(1, 300)).forgeWithSeed(seed),
  }),
});

export type CriteriaProperties = {
  freeWord: string | null;
  tags: string[] | null;
  type: ContentType | null;
  sortBy: Sort | null;
  order: Order | null;
};

export const CriteriaMold = Mold<Criteria, CriteriaProperties>({
  pour: (properties) =>
    criteriaSchema.parse({
      freeWord: properties.freeWord,
      tags: properties.tags,
      type: properties.type,
      sortBy: properties.sortBy,
      order: properties.order,
    }),
  prepare: (overrides, seed) => ({
    freeWord: overrides.freeWord ?? null,
    tags: overrides.tags ?? null,
    type: overrides.type ?? null,
    sortBy: overrides.sortBy ?? null,
    order: overrides.order ?? null,
  }),
});

export type ReferenceProperties = {
  value: ArticleIdentifier | MemoIdentifier | SeriesIdentifier;
  type: ContentType;
};

export const ReferenceMold = Mold<
  ArticleIdentifier | MemoIdentifier | SeriesIdentifier,
  ReferenceProperties
>({
  pour: (properties) => properties.value,
  prepare: (overrides, seed) => {
    const type =
      overrides.type ??
      Forger(ContentTypeMold).forgeWithSeed(seed, {
        exclusion: ContentType.ALL,
      });

    const timestamp = Forger(DateMold).forgeWithSeed(seed).getTime();

    switch (type) {
      case ContentType.ARTICLE:
        return {
          value:
            overrides.value ?? articleIdentifierSchema.parse(ulid(timestamp)),
          type,
        };
      case ContentType.MEMO:
        return {
          value: overrides.value ?? memoIdentifierSchema.parse(ulid(timestamp)),
          type,
        };
      case ContentType.SERIES:
        return {
          value:
            overrides.value ?? seriesIdentifierSchema.parse(ulid(timestamp)),
          type,
        };
      default:
        return {
          value:
            overrides.value ?? articleIdentifierSchema.parse(ulid(timestamp)),
          type: ContentType.ARTICLE,
        };
    }
  },
});

export type SearchIndexProperties = {
  identifier: SearchIndexIdentifier;
  type: ContentType;
  title: SearchIndexTitle;
  excerpt: SearchIndexExcerpt;
  tags: TagIdentifier[];
  reference: ArticleIdentifier | MemoIdentifier | SeriesIdentifier;
  timeline: Timeline;
};

export const SearchIndexMold = Mold<SearchIndex, SearchIndexProperties>({
  pour: (properties) =>
    searchIndexSchema.parse({
      identifier: properties.identifier,
      type: properties.type,
      title: properties.title,
      excerpt: properties.excerpt,
      tags: properties.tags,
      reference: properties.reference,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => {
    const type =
      overrides.type ??
      Forger(ContentTypeMold).forgeWithSeed(seed, {
        exclusion: ContentType.ALL,
      });

    const referenceResult = Forger(ReferenceMold).forgeWithSeed(seed, {
      type,
    });

    return {
      identifier:
        overrides.identifier ??
        Forger(SearchIndexIdentifierMold).forgeWithSeed(seed),
      type,
      title:
        overrides.title ?? Forger(SearchIndexTitleMold).forgeWithSeed(seed),
      excerpt:
        overrides.excerpt ?? Forger(SearchIndexExcerptMold).forgeWithSeed(seed),
      tags:
        overrides.tags ?? Forger(TagIdentifierMold).forgeMultiWithSeed(3, seed),
      reference: overrides.reference ?? referenceResult,
      timeline: overrides.timeline ?? Forger(TimelineMold).forgeWithSeed(seed),
    };
  },
});

export type SearchIndexRepositoryProperties = {
  instances: SearchIndex[];
};

export const SearchIndexRepositoryMold = Mold<
  SearchIndexRepository,
  SearchIndexRepositoryProperties
>({
  pour: (properties) => {
    const instances = properties.instances;

    const search: SearchIndexRepository["search"] = (criteria: Criteria) =>
      fromPromise<SearchIndex[], UnexpectedError>(
        new Promise((resolve) => {
          const results = instances.filter((index) => {
            // ContentType filter
            if (
              criteria.type &&
              criteria.type !== ContentType.ALL &&
              index.type !== criteria.type
            ) {
              return false;
            }

            // FreeWord filter
            if (criteria.freeWord) {
              const keyword = criteria.freeWord.toLowerCase();
              if (
                !(
                  index.title.toLowerCase().includes(keyword) ||
                  index.excerpt.toLowerCase().includes(keyword)
                )
              ) {
                return false;
              }
            }

            // Tags filter
            if (criteria.tags && criteria.tags.length > 0) {
              const hasTag = criteria.tags.some((tag) =>
                index.tags.includes(tag)
              );

              if (!hasTag) {
                return false;
              }
            }

            return true;
          });

          if (criteria.sortBy) {
            results.sort((a, b) => {
              const aTime = a.timeline.updatedAt.getTime();
              const bTime = b.timeline.updatedAt.getTime();

              if (criteria.order === Order.ASC) {
                return aTime - bTime;
              }

              return bTime - aTime;
            });
          }

          resolve(results);
        }),
        (error) => unexpectedError("Failed to search indices.", error)
      );

    return {
      search,
    };
  },
  prepare: (overrides, seed) => ({
    instances:
      overrides.instances ??
      Forger(SearchIndexMold).forgeMultiWithSeed(10, seed),
  }),
});
