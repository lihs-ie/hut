import { Image, imageSchema } from "@shared/domains/common/image";
import { Mold } from "@lihs-ie/forger-ts";

export type ImageProperties = {
  value: string;
};

export const ImageMold = Mold<Image, ImageProperties>({
  pour: (properties) => imageSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? `https://picsum.photos/seed/${seed}/600/400`,
  }),
});
