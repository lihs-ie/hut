"use server";

import { cache } from "react";
import { Memo, MemoEntry } from "@shared/domains/memo";
import { MemoWorkflowProvider } from "@/providers/workflows/memo";
import { unwrapForNextJs } from "@shared/components/global/next-error";

export const findBySlug = cache(async (slug: string): Promise<Memo> => {
  return await unwrapForNextJs(MemoWorkflowProvider.findBySlug(slug));
});

export const getEntriesBySlug = cache(
  async (slug: string): Promise<MemoEntry[]> => {
    return await unwrapForNextJs(
      MemoWorkflowProvider.findBySlug(slug).map((memo) => memo.entries),
    );
  },
);
