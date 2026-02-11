import { ulid } from "ulid";
import {
  Tag,
  TagIdentifier,
  tagIdentifierSchema,
  TagName,
  tagNameSchema,
  tagSchema,
} from "@shared/domains/attributes/tag";
import { Forger, Mold, StringMold } from "@lihs-ie/forger-ts";
import { Image } from "@shared/domains/common/image";
import { Timeline } from "@shared/domains/common";
import { TimelineMold } from "../common/date";
import { ImageMold } from "../common/image";

export type TagIdentifierProperties = {
  value: string;
};

export const TagIdentifierMold = Mold<TagIdentifier, TagIdentifierProperties>({
  pour: (properties) => tagIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? ulid(seed % 1000000),
  }),
});

export type TagNameProperties = {
  value: string;
};

export const TagNameMold = Mold<TagName, TagNameProperties>({
  pour: (properties) => tagNameSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Forger(StringMold(1, 20)).forgeWithSeed(seed),
  }),
});

export type TagProperties = {
  identifier: TagIdentifier;
  name: TagName;
  logo: Image;
  timeline: Timeline;
};

export const TagMold = Mold<Tag, TagProperties>({
  pour: (properties) =>
    tagSchema.parse({
      identifier: properties.identifier,
      name: properties.name,
      logo: properties.logo,
      timeline: properties.timeline,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ?? Forger(TagIdentifierMold).forgeWithSeed(seed),
    name: overrides.name ?? Forger(TagNameMold).forgeWithSeed(seed),
    logo: overrides.logo ?? Forger(ImageMold).forgeWithSeed(seed),
    timeline: overrides.timeline ?? Forger(TimelineMold).forgeWithSeed(seed),
  }),
});

export const TagAttributeFactory = TagMold;
