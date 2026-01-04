import {
  Memo,
  MemoEntry,
  MemoIdentifier,
  memoIdentifierSchema,
  MemoRepository,
  memoSchema,
  MemoTitle,
  titleSchema,
  entrySchema,
  Criteria,
} from "@/domains/memo";
import { Builder, Factory, StringFactory } from "../../builder";
import { ulid } from "ulid";
import { DateFactory, TimelineFactory } from "../common/date";
import { ImmutableList, ImmutableMap, Tag, Timeline } from "@/domains/common";
import { fromPromise } from "@/aspects/result";
import {
  AggregateNotFoundError,
  aggregateNotFoundError,
  DuplicationError,
  unexpectedError,
  UnexpectedError,
} from "@/aspects/error";

export type MemoIdentifierProperties = {
  value: string;
};

export const MemoIdentifierFactory = Factory<
  MemoIdentifier,
  MemoIdentifierProperties
>({
  instantiate: (properties) => memoIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Builder(DateFactory).buildWith(seed).getTime()),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type MemoTitleProperties = {
  value: string;
};

export const MemoTitleFactory = Factory<MemoTitle, MemoTitleProperties>({
  instantiate: (properties) => titleSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Builder(StringFactory(1, 100)).buildWith(seed),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type MemoEntryProperties = {
  text: string;
  createdAt: Date;
};

export const MemoEntryFactory = Factory<MemoEntry, MemoEntryProperties>({
  instantiate: (properties) =>
    entrySchema.parse({
      text: properties.text,
      createdAt: properties.createdAt,
    }),
  prepare: (overrides, seed) => ({
    text: overrides.text ?? Builder(StringFactory(1, 1000)).buildWith(seed),
    createdAt: overrides.createdAt ?? Builder(DateFactory).buildWith(seed),
  }),
  retrieve: (instance) => ({
    text: instance.text,
    createdAt: instance.createdAt,
  }),
});

export type MemoProperties = {
  identifier: MemoIdentifier;
  title: MemoTitle;
  entries: MemoEntry[];
  tags: Tag[];
  timeline: Timeline;
};

export const MemoFactory = Factory<Memo, MemoProperties>({
  instantiate: (properties) =>
    memoSchema.parse({
      identifier: properties.identifier,
      title: properties.title,
      entries: properties.entries,
      tags: properties.tags,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ?? Builder(MemoIdentifierFactory).buildWith(seed),
    title: overrides.title ?? Builder(MemoTitleFactory).buildWith(seed),
    entries:
      overrides.entries ??
      Builder(MemoEntryFactory).buildListWith(3, seed).toArray(),
    tags: overrides.tags ?? [],
    timeline: overrides.timeline ?? Builder(TimelineFactory).buildWith(seed),
  }),
  retrieve: (instance) => ({
    identifier: instance.identifier,
    title: instance.title,
    entries: instance.entries,
    tags: instance.tags,
    timeline: instance.timeline,
  }),
});

export type MemoRepositoryFactoryProperties = {
  instancies: ImmutableList<Memo>;
  onPersist?: (memo: Memo) => void;
  onTerminate?: (memo: Memo) => void;
};

export const MemoRepositoryFactory = Factory<
  MemoRepository,
  MemoRepositoryFactoryProperties
>({
  instantiate: (properties) => {
    let instancies = ImmutableMap.fromArray(
      properties.instancies
        .map((memo): [MemoIdentifier, Memo] => [memo.identifier, memo])
        .toArray()
    );

    const persist: MemoRepository["persist"] = (memo: Memo) =>
      fromPromise<void, UnexpectedError | DuplicationError<"Memo">>(
        new Promise((resolve) => {
          instancies = instancies.add(memo.identifier, memo);

          properties.onPersist?.(memo);

          resolve();
        }),
        (error) => unexpectedError("Failed to persist memo.", error)
      );

    const find: MemoRepository["find"] = (identifier) =>
      fromPromise<Memo, AggregateNotFoundError<"Memo">>(
        new Promise((resolve, reject) => {
          instancies.get(identifier).ifPresentOrElse(
            (memo) => resolve(memo),
            () =>
              reject(
                aggregateNotFoundError(
                  "Memo",
                  `Memo with identifier ${identifier} not found.`
                )
              )
          );
        }),
        (error) => error as AggregateNotFoundError<"Memo">
      );

    const terminate: MemoRepository["terminate"] = (identifier) =>
      fromPromise<void, AggregateNotFoundError<"Memo">>(
        new Promise((resolve, reject) => {
          const memoOption = instancies.get(identifier);

          memoOption.ifPresentOrElse(
            (memo) => {
              instancies = instancies.remove(identifier);

              properties.onTerminate?.(memo);

              resolve();
            },
            () =>
              reject(
                aggregateNotFoundError(
                  "Memo",
                  `Memo with identifier ${identifier} not found.`
                )
              )
          );
        }),
        (error) => error as AggregateNotFoundError<"Memo">
      );

    const search: MemoRepository["search"] = (criteria: Criteria) =>
      fromPromise<Memo[], UnexpectedError>(
        new Promise((resolve) => {
          const results = instancies
            .filter((_, memo) => {
              if (criteria.tag && !memo.tags.includes(criteria.tag)) {
                return false;
              }

              if (criteria.freeWord) {
                const keyword = criteria.freeWord.toLowerCase();
                if (
                  !(
                    memo.title.toLowerCase().includes(keyword) ||
                    memo.entries.some((entry) =>
                      entry.text.toLowerCase().includes(keyword)
                    )
                  )
                ) {
                  return false;
                }
              }

              return true;
            })
            .values();

          resolve(results);
        }),
        (error) => unexpectedError("Failed to search memos.", error)
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
      overrides.instancies ?? Builder(MemoFactory).buildListWith(10, seed),
    onPersist: overrides.onPersist,
    onTerminate: overrides.onTerminate,
  }),
  retrieve: () => {
    throw new Error("Repository cannot be retrieved.");
  },
});
