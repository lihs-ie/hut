import {
  Series,
  SeriesIdentifier,
  seriesIdentifierSchema,
  SeriesRepository,
  seriesSchema,
  Title,
  titleSchema,
  SubTitle,
  subTitleSchema,
  Description,
  descriptionSchema,
  Cover,
  cover,
  SeriesSlug,
  ChapterIdentifier,
  chapterIdentifierSchema,
} from "@shared/domains/series";
import { PublishStatus } from "@shared/domains/common";
import { ulid } from "ulid";
import { DateMold, TimelineMold } from "../common/date";
import { ImmutableMap, Timeline } from "@shared/domains/common";
import { fromPromise } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  aggregateNotFoundError,
  DuplicationError,
  unexpectedError,
  UnexpectedError,
} from "@shared/aspects/error";
import { Forger, Mold, StringMold } from "@lihs-ie/forger-ts";
import { TagIdentifier } from "@shared/domains/attributes/tag";
import {
  SlugMold,
  SlugProperties,
} from "../common/slug";

export type SeriesIdentifierProperties = {
  value: string;
};

export const SeriesIdentifierMold = Mold<
  SeriesIdentifier,
  SeriesIdentifierProperties
>({
  pour: (properties) => seriesIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Forger(DateMold).forgeWithSeed(seed).getTime()),
  }),
});

export type SeriesTitleProperties = {
  value: string;
};

export const SeriesTitleMold = Mold<Title, SeriesTitleProperties>({
  pour: (properties) => titleSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Forger(StringMold(1, 100)).forgeWithSeed(seed),
  }),
});

export type SeriesSubTitleProperties = {
  value: string | null;
};

export const SeriesSubTitleMold = Mold<SubTitle | null, SeriesSubTitleProperties>({
  pour: (properties) =>
    properties.value ? subTitleSchema.parse(properties.value) : null,
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Forger(StringMold(1, 200)).forgeWithSeed(seed),
  }),
});

export type SeriesDescriptionProperties = {
  value: string | undefined;
};

export const SeriesDescriptionMold = Mold<
  Description,
  SeriesDescriptionProperties
>({
  pour: (properties) => descriptionSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Forger(StringMold(0, 500)).forgeWithSeed(seed),
  }),
});

export type SeriesCoverProperties = {
  value: string | null;
};

export const SeriesCoverMold = Mold<Cover | null, SeriesCoverProperties>({
  pour: (properties) =>
    properties.value ? cover.parse(properties.value) : null,
  prepare: (overrides, seed) => ({
    value: overrides.value ?? `https://example.com/cover-${seed}.png`,
  }),
});

export const SeriesSlugMold = SlugMold;
export type SeriesSlugProperties = SlugProperties;

export const ChapterSlugMold = SlugMold;
export type ChapterSlugProperties = SlugProperties;

export type ChapterIdentifierProperties = {
  value: string;
};

export const ChapterIdentifierMold = Mold<
  ChapterIdentifier,
  ChapterIdentifierProperties
>({
  pour: (properties) => chapterIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Forger(DateMold).forgeWithSeed(seed).getTime()),
  }),
});

export type SeriesProperties = {
  identifier: SeriesIdentifier;
  title: Title;
  slug: SeriesSlug;
  tags: TagIdentifier[];
  subTitle: SubTitle | null;
  description: Description;
  cover: Cover | null;
  chapters: ChapterIdentifier[];
  status: PublishStatus;
  timeline: Timeline;
};

export const SeriesMold = Mold<Series, SeriesProperties>({
  pour: (properties) =>
    seriesSchema.parse({
      identifier: properties.identifier,
      title: properties.title,
      tags: properties.tags,
      slug: properties.slug,
      subTitle: properties.subTitle,
      description: properties.description,
      cover: properties.cover,
      chapters: properties.chapters,
      status: properties.status,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ?? Forger(SeriesIdentifierMold).forgeWithSeed(seed),
    title: overrides.title ?? Forger(SeriesTitleMold).forgeWithSeed(seed),
    tags: overrides.tags ?? [],
    slug: overrides.slug ?? Forger(SeriesSlugMold).forgeWithSeed(seed),
    subTitle:
      overrides.subTitle ?? Forger(SeriesSubTitleMold).forgeWithSeed(seed),
    description:
      overrides.description ??
      Forger(SeriesDescriptionMold).forgeWithSeed(seed),
    cover: overrides.cover ?? Forger(SeriesCoverMold).forgeWithSeed(seed),
    chapters:
      overrides.chapters ?? Forger(ChapterIdentifierMold).forgeMultiWithSeed(3, seed),
    status: overrides.status ?? PublishStatus.PUBLISHED,
    timeline: overrides.timeline ?? Forger(TimelineMold).forgeWithSeed(seed),
  }),
});


export type SeriesRepositoryMoldProperties = {
  instances: Series[];
  onPersist?: (series: Series) => void;
  onTerminate?: (series: Series) => void;
};

export const SeriesRepositoryMold = Mold<
  SeriesRepository,
  SeriesRepositoryMoldProperties
>({
  pour: (properties) => {
    let instances = ImmutableMap.fromArray(
      properties.instances.map((series): [SeriesIdentifier, Series] => [
        series.identifier,
        series,
      ])
    );

    const persist: SeriesRepository["persist"] = (series: Series) =>
      fromPromise<void, UnexpectedError | DuplicationError<"Series">>(
        new Promise((resolve) => {
          instances = instances.add(series.identifier, series);

          properties.onPersist?.(series);

          resolve();
        }),
        (error) => unexpectedError("Failed to persist series.", error)
      );

    const find: SeriesRepository["find"] = (identifier) =>
      fromPromise<Series, AggregateNotFoundError<"Series">>(
        new Promise((resolve, reject) => {
          instances.get(identifier).ifPresentOrElse(
            (series) => resolve(series),
            () =>
              reject(
                aggregateNotFoundError(
                  "Series",
                  `Series with identifier ${identifier} not found.`
                )
              )
          );
        }),
        (error) => error as AggregateNotFoundError<"Series">
      );

    const ofIdentifiers: SeriesRepository["ofIdentifiers"] = (
      identifiers: SeriesIdentifier[],
      throwOnMissing = false
    ) =>
      fromPromise<Series[], UnexpectedError | AggregateNotFoundError<"Series">>(
        new Promise((resolve, reject) => {
          const seriesList: Series[] = [];

          for (const identifier of identifiers) {
            const target = instances.get(identifier);

            if (!target.isPresent()) {
              if (throwOnMissing) {
                reject(
                  aggregateNotFoundError(
                    "Series",
                    `Series with identifier ${identifier} not found.`
                  )
                );
                return;
              }
              continue;
            }

            target.ifPresent((series) => {
              seriesList.push(series);
            });
          }

          resolve(seriesList);
        }),
        (error) => error as UnexpectedError | AggregateNotFoundError<"Series">
      );

    const terminate: SeriesRepository["terminate"] = (identifier) =>
      fromPromise<void, AggregateNotFoundError<"Series">>(
        new Promise((resolve, reject) => {
          const target = instances.get(identifier);

          target.ifPresentOrElse(
            (series) => {
              instances = instances.remove(identifier);
              properties.onTerminate?.(series);

              resolve();
            },
            () =>
              reject(
                aggregateNotFoundError(
                  "Series",
                  `Series with identifier ${identifier} not found.`
                )
              )
          );
        }),
        (error) => error as AggregateNotFoundError<"Series">
      );

    const findBySlug: SeriesRepository["findBySlug"] = (slug) =>
      fromPromise<Series, AggregateNotFoundError<"Series"> | UnexpectedError>(
        new Promise((resolve, reject) => {
          const found = instances
            .values()
            .find((series) => series.slug === slug);

          if (found) {
            resolve(found);
          } else {
            reject(
              aggregateNotFoundError(
                "Series",
                `Series with slug ${slug} not found.`
              )
            );
          }
        }),
        (error) => error as AggregateNotFoundError<"Series"> | UnexpectedError
      );

    const search: SeriesRepository["search"] = (criteria) =>
      fromPromise<Series[], UnexpectedError>(
        new Promise((resolve) => {
          const results = instances
            .filter((_, series) => {
              if (criteria.slug && series.slug !== criteria.slug) {
                return false;
              }

              if (criteria.status && series.status !== criteria.status) {
                return false;
              }

              if (criteria.tags && criteria.tags.length > 0) {
                const hasTag = criteria.tags.some((tag) =>
                  series.tags.includes(tag)
                );
                if (!hasTag) {
                  return false;
                }
              }

              return true;
            })
            .values();

          const filtered = criteria.freeWord
            ? results.filter((series) =>
                series.title.toLowerCase().includes(criteria.freeWord!.toLowerCase())
              )
            : results;

          resolve(filtered);
        }),
        (error) => unexpectedError("Failed to search series.", error)
      );

    return {
      find,
      findBySlug,
      persist,
      ofIdentifiers,
      terminate,
      search,
    };
  },
  prepare: (overrides, seed) => ({
    instances:
      overrides.instances ?? Forger(SeriesMold).forgeMultiWithSeed(10, seed),
    onPersist: overrides.onPersist,
    onTerminate: overrides.onTerminate,
  }),
});
