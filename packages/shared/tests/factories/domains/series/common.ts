import {
  Series,
  SeriesIdentifier,
  seriesIdentifierSchema,
  SeriesRepository,
  seriesSchema,
  Title,
  titleSchema,
  Description,
  descriptionSchema,
  Cover,
  cover,
  Page,
  pageSchema,
} from "@/domains/series";
import { Builder, Factory, StringFactory } from "../../builder";
import { ulid } from "ulid";
import { DateFactory, TimelineFactory } from "../common/date";
import { ImmutableList, ImmutableMap, Timeline } from "@/domains/common";
import { fromPromise } from "@/aspects/result";
import {
  AggregateNotFoundError,
  aggregateNotFoundError,
  DuplicationError,
  unexpectedError,
  UnexpectedError,
} from "@/aspects/error";

export type SeriesIdentifierProperties = {
  value: string;
};

export const SeriesIdentifierFactory = Factory<
  SeriesIdentifier,
  SeriesIdentifierProperties
>({
  instantiate: (properties) => seriesIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Builder(DateFactory).buildWith(seed).getTime()),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type SeriesTitleProperties = {
  value: string;
};

export const SeriesTitleFactory = Factory<Title, SeriesTitleProperties>({
  instantiate: (properties) => titleSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Builder(StringFactory(1, 100)).buildWith(seed),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type SeriesDescriptionProperties = {
  value: string | undefined;
};

export const SeriesDescriptionFactory = Factory<
  Description,
  SeriesDescriptionProperties
>({
  instantiate: (properties) => descriptionSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Builder(StringFactory(0, 500)).buildWith(seed),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type SeriesCoverProperties = {
  value: string | null;
};

export const SeriesCoverFactory = Factory<Cover | null, SeriesCoverProperties>({
  instantiate: (properties) =>
    properties.value ? cover.parse(properties.value) : null,
  prepare: (overrides, seed) => ({
    value: overrides.value ?? `https://example.com/cover-${seed}.png`,
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type PageProperties = {
  title: string;
  content: string;
  timeline: Timeline;
};

export const PageFactory = Factory<Page, PageProperties>({
  instantiate: (properties) =>
    pageSchema.parse({
      title: properties.title,
      content: properties.content,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => ({
    title: overrides.title ?? Builder(StringFactory(1, 100)).buildWith(seed),
    content:
      overrides.content ?? Builder(StringFactory(1, 1000)).buildWith(seed),
    timeline: overrides.timeline ?? Builder(TimelineFactory).buildWith(seed),
  }),
  retrieve: (instance) => ({
    title: instance.title,
    content: instance.content,
    timeline: instance.timeline,
  }),
});

export type SeriesProperties = {
  identifier: SeriesIdentifier;
  title: Title;
  description: Description;
  cover: Cover | null;
  pages: Page[];
  timeline: Timeline;
};

export const SeriesFactory = Factory<Series, SeriesProperties>({
  instantiate: (properties) =>
    seriesSchema.parse({
      identifier: properties.identifier,
      title: properties.title,
      description: properties.description,
      cover: properties.cover,
      pages: properties.pages,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ?? Builder(SeriesIdentifierFactory).buildWith(seed),
    title: overrides.title ?? Builder(SeriesTitleFactory).buildWith(seed),
    description:
      overrides.description ??
      Builder(SeriesDescriptionFactory).buildWith(seed),
    cover: overrides.cover ?? Builder(SeriesCoverFactory).buildWith(seed),
    pages:
      overrides.pages ?? Builder(PageFactory).buildListWith(3, seed).toArray(),
    timeline: overrides.timeline ?? Builder(TimelineFactory).buildWith(seed),
  }),
  retrieve: (instance) => ({
    identifier: instance.identifier,
    title: instance.title,
    description: instance.description,
    cover: instance.cover,
    pages: instance.pages,
    timeline: instance.timeline,
  }),
});

export type SeriesRepositoryFactoryProperties = {
  instancies: ImmutableList<Series>;
  onPersist?: (series: Series) => void;
  onTerminate?: (series: Series) => void;
};

export const SeriesRepositoryFactory = Factory<
  SeriesRepository,
  SeriesRepositoryFactoryProperties
>({
  instantiate: (properties) => {
    let instancies = ImmutableMap.fromArray(
      properties.instancies
        .map((series): [SeriesIdentifier, Series] => [
          series.identifier,
          series,
        ])
        .toArray()
    );

    const persist: SeriesRepository["persist"] = (series: Series) =>
      fromPromise<void, UnexpectedError | DuplicationError<"Series">>(
        new Promise((resolve) => {
          instancies = instancies.add(series.identifier, series);

          properties.onPersist?.(series);

          resolve();
        }),
        (error) => unexpectedError("Failed to persist series.", error)
      );

    const find: SeriesRepository["find"] = (identifier) =>
      fromPromise<Series, AggregateNotFoundError<"Series">>(
        new Promise((resolve, reject) => {
          instancies.get(identifier).ifPresentOrElse(
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

    const terminate: SeriesRepository["terminate"] = (identifier) =>
      fromPromise<void, AggregateNotFoundError<"Series">>(
        new Promise((resolve, reject) => {
          const seriesOption = instancies.get(identifier);

          seriesOption.ifPresentOrElse(
            (series) => {
              instancies = instancies.remove(identifier);

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

    const search: SeriesRepository["search"] = (title: string) =>
      fromPromise<Series[], UnexpectedError>(
        new Promise((resolve) => {
          const results = instancies
            .filter((_, series) => {
              if (title) {
                const keyword = title.toLowerCase();
                if (!series.title.toLowerCase().includes(keyword)) {
                  return false;
                }
              }

              return true;
            })
            .values();

          resolve(results);
        }),
        (error) => unexpectedError("Failed to search series.", error)
      );

    return {
      find,
      persist,
      terminate,
      search,
    };
  },
  prepare: (overrides, seed) => ({
    instancies:
      overrides.instancies ?? Builder(SeriesFactory).buildListWith(10, seed),
    onPersist: overrides.onPersist,
    onTerminate: overrides.onTerminate,
  }),
  retrieve: () => {
    throw new Error("Repository cannot be retrieved.");
  },
});
