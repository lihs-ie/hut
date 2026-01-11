import {
  ExternalServiceType,
  ExternalService,
  externalServiceSchema,
} from "@shared/domains/common/service";
import { EnumMold, Forger, Mold, StringMold } from "@lihs-ie/forger-ts";

export const ExternalServiceTypeMold = EnumMold(ExternalServiceType);

export type ExternalServiceProperties = {
  type: ExternalServiceType;
  user: string;
};

export const ExternalServiceMold = Mold<
  ExternalService,
  ExternalServiceProperties
>({
  pour: (properties) => externalServiceSchema.parse(properties),
  prepare: (overrides, seed) => ({
    type: overrides.type ?? Forger(ExternalServiceTypeMold).forgeWithSeed(seed),
    user: overrides.user ?? Forger(StringMold(1, 50)).forgeWithSeed(seed),
  }),
});
