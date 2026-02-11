import { Pager, pagerSchema } from "@shared/domains/common/pager";
import { Mold } from "@lihs-ie/forger-ts";

type PagerProperties = {
  total: number;
  items: number;
  current: number;
};

export const PagerMold = Mold<Pager, PagerProperties>({
  pour: (properties) => pagerSchema.parse(properties),
  prepare: (overrides, seed) => {
    const total = Math.trunc(seed % 1000);
    const items = Math.trunc(seed % 10) + 1;
    const current = Math.ceil(total / items);

    return {
      total: total,
      items: items,
      current: current,
      ...overrides,
    };
  },
});
