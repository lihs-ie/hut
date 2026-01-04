import { Pager, pagerSchema } from "@/domains/common/pager";
import { Factory } from "../../builder";

type PagerProperties = {
  total: number;
  items: number;
  current: number;
};

export const PagerFactory = Factory<Pager, PagerProperties>({
  instantiate: (properties) => pagerSchema.parse(properties),
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
  retrieve: (instance) => ({
    total: instance.total,
    items: instance.items,
    current: instance.current,
  }),
});
