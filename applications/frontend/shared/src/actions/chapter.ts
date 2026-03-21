"use server";

import { cache } from "react";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Chapter, ChapterIdentifier } from "@shared/domains/series/chapter";
import { ChapterWorkflowProvider } from "@shared/providers/workflows/chapter";
import { ChapterRepositoryProvider } from "@shared/providers/infrastructure/chapter";

export const findChapterBySlug = cache(
  async (slug: string): Promise<Chapter> => {
    return await unwrapForNextJs(
      ChapterWorkflowProvider.findBySlug({ payload: { slug }, now: new Date() }),
    );
  },
);

export const findChaptersByIdentifiers = cache(
  async (identifiers: ChapterIdentifier[]): Promise<Chapter[]> => {
    return await unwrapForNextJs(
      ChapterRepositoryProvider.firebase.ofIdentifiers(identifiers),
    );
  },
);
