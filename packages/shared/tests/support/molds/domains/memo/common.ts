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
  MemoSlug,
} from "@shared/domains/memo";
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

export type MemoIdentifierProperties = {
  value: string;
};

export const MemoIdentifierMold = Mold<
  MemoIdentifier,
  MemoIdentifierProperties
>({
  pour: (properties) => memoIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Forger(DateMold).forgeWithSeed(seed).getTime()),
  }),
});

export type MemoTitleProperties = {
  value: string;
};

export const MemoTitleMold = Mold<MemoTitle, MemoTitleProperties>({
  pour: (properties) => titleSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Forger(StringMold(1, 100)).forgeWithSeed(seed),
  }),
});

export type MemoEntryProperties = {
  text: string;
  createdAt: Date;
};

export const MemoEntryMold = Mold<MemoEntry, MemoEntryProperties>({
  pour: (properties) =>
    entrySchema.parse({
      text: properties.text,
      createdAt: properties.createdAt,
    }),
  prepare: (overrides, seed) => ({
    text: overrides.text ?? Forger(StringMold(1, 1000)).forgeWithSeed(seed),
    createdAt: overrides.createdAt ?? Forger(DateMold).forgeWithSeed(seed),
  }),
});

export const MemoSlugMold = SlugMold;
export type MemoSlugProperties = SlugProperties;

export type MemoProperties = {
  identifier: MemoIdentifier;
  title: MemoTitle;
  slug: MemoSlug;
  entries: MemoEntry[];
  tags: TagIdentifier[];
  timeline: Timeline;
};

export const MemoMold = Mold<Memo, MemoProperties>({
  pour: (properties) =>
    memoSchema.parse({
      identifier: properties.identifier,
      title: properties.title,
      slug: properties.slug,
      entries: properties.entries,
      tags: properties.tags,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ?? Forger(MemoIdentifierMold).forgeWithSeed(seed),
    title: overrides.title ?? Forger(MemoTitleMold).forgeWithSeed(seed),
    slug: overrides.slug ?? Forger(MemoSlugMold).forgeWithSeed(seed),
    entries:
      overrides.entries ?? Forger(MemoEntryMold).forgeMultiWithSeed(3, seed),
    tags: overrides.tags ?? [],
    timeline: overrides.timeline ?? Forger(TimelineMold).forgeWithSeed(seed),
  }),
});

export const MemoIdentifierFactory = MemoIdentifierMold;
export const MemoEntryFactory = MemoEntryMold;
export const MemoSlugFactory = MemoSlugMold;
export const MemoFactory = MemoMold;

export type MemoRepositoryMoldProperties = {
  instances: Memo[];
  onPersist?: (memo: Memo) => void;
  onTerminate?: (memo: Memo) => void;
};

export const MemoRepositoryMold = Mold<
  MemoRepository,
  MemoRepositoryMoldProperties
>({
  pour: (properties) => {
    let instances = ImmutableMap.fromArray(
      properties.instances.map((memo): [MemoIdentifier, Memo] => [
        memo.identifier,
        memo,
      ])
    );

    const persist: MemoRepository["persist"] = (memo: Memo) =>
      fromPromise<void, UnexpectedError | DuplicationError<"Memo">>(
        new Promise((resolve) => {
          instances = instances.add(memo.identifier, memo);

          properties.onPersist?.(memo);

          resolve();
        }),
        (error) => unexpectedError("Failed to persist memo.", error)
      );

    const find: MemoRepository["find"] = (identifier) =>
      fromPromise<Memo, AggregateNotFoundError<"Memo">>(
        new Promise((resolve, reject) => {
          instances.get(identifier).ifPresentOrElse(
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
          const memoOption = instances.get(identifier);

          memoOption.ifPresentOrElse(
            (memo) => {
              instances = instances.remove(identifier);
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

    const findBySlug: MemoRepository["findBySlug"] = (slug) =>
      fromPromise<Memo, AggregateNotFoundError<"Memo"> | UnexpectedError>(
        new Promise((resolve, reject) => {
          const found = instances.values().find((memo) => memo.slug === slug);

          if (found) {
            resolve(found);
          } else {
            reject(
              aggregateNotFoundError(
                "Memo",
                `Memo with slug ${slug} not found.`
              )
            );
          }
        }),
        (error) => error as AggregateNotFoundError<"Memo"> | UnexpectedError
      );

    const ofIdentifiers: MemoRepository["ofIdentifiers"] = (
      identifiers,
      throwOnMissing = false
    ) =>
      fromPromise<Memo[], UnexpectedError | AggregateNotFoundError<"Memo">>(
        new Promise((resolve, reject) => {
          const memos: Memo[] = [];

          for (const identifier of identifiers) {
            const target = instances.get(identifier);

            if (!target.isPresent()) {
              if (throwOnMissing) {
                reject(
                  aggregateNotFoundError(
                    "Memo",
                    `Memo with identifier ${identifier} not found.`
                  )
                );
                return;
              }
              continue;
            }

            target.ifPresent((memo) => {
              memos.push(memo);
            });
          }

          resolve(memos);
        }),
        (error) => error as UnexpectedError | AggregateNotFoundError<"Memo">
      );

    const search: MemoRepository["search"] = (criteria: Criteria) =>
      fromPromise<Memo[], UnexpectedError>(
        new Promise((resolve) => {
          const results = instances
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
      findBySlug,
      ofIdentifiers,
      persist,
      terminate,
      search,
    };
  },
  prepare: (overrides, seed) => ({
    instances:
      overrides.instances ?? Forger(MemoMold).forgeMultiWithSeed(10, seed),
    onPersist: overrides.onPersist,
    onTerminate: overrides.onTerminate,
  }),
});
