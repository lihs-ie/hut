"use server";

import { revalidateTag } from "next/cache";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Chapter, UnvalidatedChapter } from "@shared/domains/series/chapter";
import { AdminChapterWorkflowProvider } from "@/providers/workflows/chapter";
import { requireAdmin } from "@/aspects/auth-guard";

export async function persist(unvalidated: UnvalidatedChapter): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(AdminChapterWorkflowProvider.persist(unvalidated));

  revalidateTag("chapters", {});
}

export async function findBySlug(slug: string): Promise<Chapter> {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminChapterWorkflowProvider.findBySlug({
      payload: { slug },
      now: new Date(),
    }),
  );
}
