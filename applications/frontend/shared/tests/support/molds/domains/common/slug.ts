import { Characters, Forger, Mold, StringMold } from "@lihs-ie/forger-ts";
import { Slug, slugSchema } from "@shared/domains/common";

export type SlugProperties = {
  value: string;
};

export const SlugMold = Mold<Slug, SlugProperties>({
  pour: (properties) => slugSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ??
      Forger(StringMold(1, 100, Characters.SLUG)).forgeWithSeed(seed),
  }),
});
