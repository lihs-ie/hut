"use server";

import { cache } from "react";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Chapter, ChapterIdentifier } from "@shared/domains/series/chapter";
import { PublishStatus } from "@shared/domains/common";
import { ReaderChapterWorkflowProvider } from "@/providers/workflows/chapter";
import { ReaderChapterRepositoryProvider } from "@/providers/infrastructure/chapter";

export const findChapterBySlug = cache(
  async (slug: string): Promise<Chapter> => {
    return await unwrapForNextJs(
      ReaderChapterWorkflowProvider.findBySlug({
        payload: { slug },
        now: new Date(),
      }),
    );
  },
);

export const findPublishedChaptersByIdentifiers = cache(
  async (identifiers: ChapterIdentifier[]): Promise<Chapter[]> => {
    const chapters = await unwrapForNextJs(
      ReaderChapterRepositoryProvider.firebase.ofIdentifiers(identifiers),
    );
    return chapters.filter(
      (chapter) => chapter.status === PublishStatus.PUBLISHED,
    );
  },
);
