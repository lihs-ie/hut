import {
  TechnologyCategory,
  TechnologyKind,
  TechnologyStack,
  technologyStackSchema,
} from "@shared/domains/common/tech";
import { DateMold } from "./date";
import { EnumMold, Forger, Mold } from "@lihs-ie/forger-ts";

export const TechnologyCategoryMold = EnumMold(TechnologyCategory);

export const TechnologyKindMold = EnumMold(TechnologyKind);

export type TechnologyStackProperties = {
  kind: TechnologyKind;
  from: Date;
  continue: boolean;
};

export const TechnologyStackMold = Mold<
  TechnologyStack,
  TechnologyStackProperties
>({
  pour: (properties) => technologyStackSchema.parse(properties),
  prepare: (overrides, seed) => ({
    kind: overrides.kind ?? Forger(TechnologyKindMold).forgeWithSeed(seed),
    from: overrides.from ?? Forger(DateMold).forgeWithSeed(seed),
    continue: overrides.continue ?? seed % 2 === 0,
  }),
});

export const TechnologyStackFactory = TechnologyStackMold;
