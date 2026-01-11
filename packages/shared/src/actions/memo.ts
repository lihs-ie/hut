"use server";

import { cache } from "react";
import { Memo, MemoEntry, UnvalidatedCriteria } from "@shared/domains/memo";
import { MemoWorkflowProvider } from "@shared/providers/workflows/memo";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { PublishStatus } from "@shared/domains/common";

export const find = cache(async (identifier: string): Promise<Memo> => {
  return await unwrapForNextJs(MemoWorkflowProvider.find(identifier));
});

export const findBySlug = cache(async (slug: string): Promise<Memo> => {
  return await unwrapForNextJs(MemoWorkflowProvider.findBySlug(slug));
});

export const search = cache(
  async (unvalidated: UnvalidatedCriteria): Promise<Memo[]> => {
    return await unwrapForNextJs(
      MemoWorkflowProvider.search({
        now: new Date(),
        payload: {
          ...unvalidated,
          status: PublishStatus.PUBLISHED,
        },
      }),
    );
  },
);

export const getEntriesBySlug = cache(
  async (slug: string): Promise<MemoEntry[]> => {
    return await unwrapForNextJs(
      MemoWorkflowProvider.findBySlug(slug).map((memo) => memo.entries),
    );
  },
);
