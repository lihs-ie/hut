import {
  SearchToken,
  SearchTokenIdentifier,
  SearchTokenType,
  SearchTokenValue,
  searchTokenValueSchema,
  searchTokenSchema,
  createSearchTokenIdentifier,
  Criteria,
  criteriaSchema,
  Sort,
  Order,
  ContentType,
  SearchReference,
  searchReferenceSchema,
  SearchReferenceIdentifier,
  searchReferenceIdentifierSchema,
  SearchTokenRepository,
} from "@shared/domains/search-token";
import { ulid } from "ulid";
import { DateMold, TimelineMold } from "../common/date";
import { ImmutableMap, Timeline } from "@shared/domains/common";
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
import {
  AggregateNotFoundError,
  aggregateNotFoundError,
  DuplicationError,
  unexpectedError,
  UnexpectedError,
} from "@shared/aspects/error";
import { EnumMold, Forger, Mold, StringMold } from "@lihs-ie/forger-ts";

export const SearchTokenTypeMold = EnumMold(SearchTokenType);

export const SearchTokenContentTypeMold = EnumMold(ContentType);

export const SortMold = EnumMold(Sort);

export const OrderMold = EnumMold(Order);

export type SearchTokenValueProperties = {
  value: string;
};

export const SearchTokenValueMold = Mold<
  SearchTokenValue,
  SearchTokenValueProperties
>({
  pour: (properties) => searchTokenValueSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Forger(StringMold(1, 20)).forgeWithSeed(seed),
  }),
});

export type SearchTokenIdentifierProperties = {
  type: SearchTokenType;
  value: SearchTokenValue;
};

export const SearchTokenIdentifierMold = Mold<
  SearchTokenIdentifier,
  SearchTokenIdentifierProperties
>({
  pour: (properties) =>
    createSearchTokenIdentifier(properties.type, properties.value),
  prepare: (overrides, seed) => ({
    type:
      overrides.type ??
      (Forger(SearchTokenTypeMold).forgeWithSeed(seed) as SearchTokenType),
    value:
      overrides.value ?? Forger(SearchTokenValueMold).forgeWithSeed(seed),
  }),
});

export type SearchReferenceIdentifierProperties = {
  type: ContentType;
  content: ArticleIdentifier | MemoIdentifier | SeriesIdentifier;
};

export const SearchReferenceIdentifierMold = Mold<
  SearchReferenceIdentifier,
  SearchReferenceIdentifierProperties
>({
  pour: (properties) =>
    searchReferenceIdentifierSchema.parse({
      type: properties.type,
      content: properties.content,
    }),
  prepare: (overrides, seed) => {
    const type =
      overrides.type ??
      (Forger(SearchTokenContentTypeMold).forgeWithSeed(seed) as ContentType);

    const timestamp = Forger(DateMold).forgeWithSeed(seed).getTime();

    let content: ArticleIdentifier | MemoIdentifier | SeriesIdentifier;
    switch (type) {
      case ContentType.ARTICLE:
        content =
          overrides.content ?? articleIdentifierSchema.parse(ulid(timestamp));
        break;
      case ContentType.MEMO:
        content =
          overrides.content ?? memoIdentifierSchema.parse(ulid(timestamp));
        break;
      case ContentType.SERIES:
        content =
          overrides.content ?? seriesIdentifierSchema.parse(ulid(timestamp));
        break;
      default:
        content =
          overrides.content ?? articleIdentifierSchema.parse(ulid(timestamp));
        break;
    }

    return {
      type,
      content,
    };
  },
});

export type SearchReferenceProperties = {
  identifier: SearchReferenceIdentifier;
  score: number;
  updatedAt: Date;
};

export const SearchReferenceMold = Mold<
  SearchReference,
  SearchReferenceProperties
>({
  pour: (properties) =>
    searchReferenceSchema.parse({
      identifier: properties.identifier,
      score: properties.score,
      updatedAt: properties.updatedAt,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ??
      Forger(SearchReferenceIdentifierMold).forgeWithSeed(seed),
    score: overrides.score ?? (seed % 100) / 10,
    updatedAt: overrides.updatedAt ?? Forger(DateMold).forgeWithSeed(seed),
  }),
});

export type SearchTokenProperties = {
  identifier: SearchTokenIdentifier;
  references: SearchReference[];
  type: SearchTokenType;
  value: SearchTokenValue;
  timeline: Timeline;
};

export const SearchTokenMold = Mold<SearchToken, SearchTokenProperties>({
  pour: (properties) =>
    searchTokenSchema.parse({
      identifier: properties.identifier,
      references: properties.references,
      type: properties.type,
      value: properties.value,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => {
    const type =
      overrides.type ??
      (Forger(SearchTokenTypeMold).forgeWithSeed(seed) as SearchTokenType);
    const value =
      overrides.value ?? Forger(SearchTokenValueMold).forgeWithSeed(seed);

    return {
      identifier:
        overrides.identifier ?? createSearchTokenIdentifier(type, value),
      references:
        overrides.references ??
        Forger(SearchReferenceMold).forgeMultiWithSeed(3, seed),
      type,
      value,
      timeline: overrides.timeline ?? Forger(TimelineMold).forgeWithSeed(seed),
    };
  },
});

export type SearchTokenCriteriaProperties = {
  freeWord: string | null;
  tags: string[] | null;
  type: ContentType | null;
  sortBy: Sort | null;
  order: Order | null;
  limit: number | null;
};

export const SearchTokenCriteriaMold = Mold<
  Criteria,
  SearchTokenCriteriaProperties
>({
  pour: (properties) =>
    criteriaSchema.parse({
      freeWord: properties.freeWord,
      tags: properties.tags,
      type: properties.type,
      sortBy: properties.sortBy,
      order: properties.order,
      limit: properties.limit,
    }),
  prepare: (overrides, _seed) => ({
    freeWord: overrides.freeWord ?? null,
    tags: overrides.tags ?? null,
    type: overrides.type ?? null,
    sortBy: overrides.sortBy ?? null,
    order: overrides.order ?? null,
    limit: overrides.limit ?? null,
  }),
});

export type SearchTokenRepositoryMoldProperties = {
  instances: SearchToken[];
};

export const SearchTokenRepositoryMold = Mold<
  SearchTokenRepository,
  SearchTokenRepositoryMoldProperties
>({
  pour: (properties) => {
    let instances = ImmutableMap.fromArray(
      properties.instances.map(
        (token): [SearchTokenIdentifier, SearchToken] => [
          token.identifier,
          token,
        ]
      )
    );

    const persist: SearchTokenRepository["persist"] = (token: SearchToken) =>
      fromPromise<void, UnexpectedError | DuplicationError<"SearchToken">>(
        new Promise((resolve) => {
          instances = instances.add(token.identifier, token);
          resolve();
        }),
        (error) => unexpectedError("Failed to persist search token.", error)
      );

    const find: SearchTokenRepository["find"] = (identifier) =>
      fromPromise<
        SearchToken,
        UnexpectedError | AggregateNotFoundError<"SearchToken">
      >(
        new Promise((resolve, reject) => {
          instances.get(identifier).ifPresentOrElse(
            (token) => resolve(token),
            () =>
              reject(
                aggregateNotFoundError(
                  "SearchToken",
                  `SearchToken with identifier ${identifier} not found.`
                )
              )
          );
        }),
        (error) => error as AggregateNotFoundError<"SearchToken">
      );

    const ofIdentifiers: SearchTokenRepository["ofIdentifiers"] = (
      identifiers,
      throwOnMissing = false
    ) =>
      fromPromise<
        SearchToken[],
        UnexpectedError | AggregateNotFoundError<"SearchToken">
      >(
        new Promise((resolve, reject) => {
          const tokens: SearchToken[] = [];

          for (const identifier of identifiers) {
            const tokenOption = instances.get(identifier);

            if (!tokenOption.isPresent()) {
              if (throwOnMissing) {
                reject(
                  aggregateNotFoundError(
                    "SearchToken",
                    `SearchToken with identifier ${identifier} not found.`
                  )
                );
                return;
              }
              continue;
            }

            tokenOption.ifPresent((token) => tokens.push(token));
          }

          resolve(tokens);
        }),
        (error) =>
          error as UnexpectedError | AggregateNotFoundError<"SearchToken">
      );

    const search: SearchTokenRepository["search"] = (_criteria: Criteria) =>
      fromPromise<SearchToken[], UnexpectedError>(
        new Promise((resolve) => {
          resolve(instances.values());
        }),
        (error) => unexpectedError("Failed to search tokens.", error)
      );

    const terminate: SearchTokenRepository["terminate"] = (identifier) =>
      fromPromise<void, UnexpectedError | AggregateNotFoundError<"SearchToken">>(
        new Promise((resolve, reject) => {
          const target = instances.get(identifier);

          target.ifPresentOrElse(
            () => {
              instances = instances.remove(identifier);
              resolve();
            },
            () =>
              reject(
                aggregateNotFoundError(
                  "SearchToken",
                  `SearchToken with identifier ${identifier} not found.`
                )
              )
          );
        }),
        (error) => error as AggregateNotFoundError<"SearchToken">
      );

    return {
      persist,
      find,
      ofIdentifiers,
      search,
      terminate,
    };
  },
  prepare: (overrides, seed) => ({
    instances:
      overrides.instances ?? Forger(SearchTokenMold).forgeMultiWithSeed(10, seed),
  }),
});
