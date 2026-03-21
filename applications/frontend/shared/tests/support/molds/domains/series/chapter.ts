import {
  Chapter,
  ChapterIdentifier,
  chapterIdentifierSchema,
  chapterSchema,
} from "@shared/domains/series/chapter";
import { ulid } from "ulid";
import { Forger, Mold, StringMold } from "@lihs-ie/forger-ts";
import { DateMold, TimelineMold } from "../common/date";
import { SlugMold } from "../common/slug";
import { PublishStatusMold } from "../common/status";
import type { Slug, Timeline } from "@shared/domains/common";
import type { PublishStatus } from "@shared/domains/common";
import type { ImageIdentifier } from "@shared/domains/image";

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

export type ChapterProperties = {
  identifier: ChapterIdentifier;
  title: string;
  slug: Slug;
  content: string;
  images: ImageIdentifier[];
  status: PublishStatus;
  timeline: Timeline;
};

export const ChapterMold = Mold<Chapter, ChapterProperties>({
  pour: (properties) =>
    chapterSchema.parse({
      identifier: properties.identifier,
      title: properties.title,
      slug: properties.slug,
      content: properties.content,
      images: properties.images,
      status: properties.status,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ?? Forger(ChapterIdentifierMold).forgeWithSeed(seed),
    title: overrides.title ?? Forger(StringMold(1, 100)).forgeWithSeed(seed),
    slug: overrides.slug ?? Forger(SlugMold).forgeWithSeed(seed),
    content:
      overrides.content ?? Forger(StringMold(1, 1000)).forgeWithSeed(seed),
    images: overrides.images ?? [],
    status:
      overrides.status ?? Forger(PublishStatusMold).forgeWithSeed(seed),
    timeline: overrides.timeline ?? Forger(TimelineMold).forgeWithSeed(seed),
  }),
});
