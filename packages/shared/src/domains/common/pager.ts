import z from "zod";

export const pagerSchema = z
  .object({
    total: z.number().min(0),
    items: z.number().min(0),
    current: z.number().min(0),
  })
  .brand("Pager");

export type Pager = z.infer<typeof pagerSchema>;

export const offset = (pager: Pager): number => {
  return (pager.current - 1) * pager.items;
};

export const first = (pager: Pager): number => {
  return pager.total === 0 ? 0 : 1;
};

export const last = (pager: Pager): number => {
  return Math.ceil(pager.total / pager.items);
};
