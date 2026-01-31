import {
  ExperienceType,
  TechnologyCategory,
  TechnologyStack,
  technologyStackSchema,
} from "@shared/domains/common/tech";
import { TagIdentifier } from "@shared/domains/attributes/tag";
import { DateMold } from "./date";
import { EnumMold, Forger, Mold } from "@lihs-ie/forger-ts";
import { TagIdentifierMold } from "../attributes/tag";

export const TechnologyCategoryMold = EnumMold(TechnologyCategory);

export const ExperienceTypeMold = EnumMold(ExperienceType);

export type TechnologyStackProperties = {
  tag: TagIdentifier;
  from: Date;
  continue: boolean;
  type: ExperienceType;
};

export const TechnologyStackMold = Mold<
  TechnologyStack,
  TechnologyStackProperties
>({
  pour: (properties) => technologyStackSchema.parse(properties),
  prepare: (overrides, seed) => ({
    tag:
      overrides.tag ??
      Forger(TagIdentifierMold).forgeWithSeed(seed),
    from: overrides.from ?? Forger(DateMold).forgeWithSeed(seed),
    continue: overrides.continue ?? seed % 2 === 0,
    type: overrides.type ?? Forger(ExperienceTypeMold).forgeWithSeed(seed),
  }),
});

export const TechnologyStackFactory = TechnologyStackMold;
