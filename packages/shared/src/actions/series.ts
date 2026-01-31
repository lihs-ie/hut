"use server";

import { cache } from "react";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Series, UnvalidatedCriteria } from "@shared/domains/series";
import { SeriesWorkflowProvider } from "@shared/providers/workflows/series";

export const findBySlug = cache(async (slug: string): Promise<Series> => {
  return await unwrapForNextJs(
    SeriesWorkflowProvider.findBySlug({ payload: { slug }, now: new Date() }),
  );
});

export const find = cache(async (identifier: string): Promise<Series> => {
  return await unwrapForNextJs(SeriesWorkflowProvider.find(identifier));
});

export const search = cache(
  async (unvalidated: UnvalidatedCriteria): Promise<Series[]> => {
    return await unwrapForNextJs(SeriesWorkflowProvider.search(unvalidated));
  },
);
