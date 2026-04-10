"use server";

import {
  Memo,
  MemoEntry,
  MemoSnapshot,
  UnvalidatedCriteria,
  UnvalidatedEntry,
  UnvalidatedMemo,
} from "@shared/domains/memo";
import { ImageIdentifier } from "@shared/domains/image";
import { AdminMemoWorkflowProvider } from "@/providers/workflows/memo";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { revalidateTag } from "next/cache";
import { EventBrokerProvider } from "@/providers/domain/event";
import { requireAdmin } from "@/aspects/auth-guard";
import { notifyReaderRevalidation } from "@/aspects/revalidation";
import { REVALIDATION_TAGS, memoEntriesTag } from "@shared/config/revalidation";

export async function findBySlug(slug: string): Promise<Memo> {
  await requireAdmin();
  return await unwrapForNextJs(AdminMemoWorkflowProvider.findBySlug(slug));
}

export async function getEntriesBySlug(slug: string): Promise<MemoEntry[]> {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminMemoWorkflowProvider.findBySlug(slug).map((memo) => memo.entries),
  );
}

export async function create(unvalidated: UnvalidatedMemo): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(
    AdminMemoWorkflowProvider.create({
      now: new Date(),
      payload: { unvalidated },
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("memos", {});
  await notifyReaderRevalidation([REVALIDATION_TAGS.MEMOS]);
}

export async function addEntry(
  unvalidated: UnvalidatedEntry,
  slug: string,
  images: ImageIdentifier[],
): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(
    AdminMemoWorkflowProvider.addEntry({
      now: new Date(),
      payload: { unvalidated, slug, images },
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag(`memo-entries-${slug}`, { expire: 3600 });
  await notifyReaderRevalidation([memoEntriesTag(slug)]);
}

export async function edit(
  unvalidated: UnvalidatedMemo,
  before: MemoSnapshot,
): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(
    AdminMemoWorkflowProvider.edit({
      now: new Date(),
      payload: { unvalidated, before },
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("memos", {});
  await notifyReaderRevalidation([REVALIDATION_TAGS.MEMOS]);
}

export async function search(
  unvalidated: UnvalidatedCriteria,
): Promise<Memo[]> {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminMemoWorkflowProvider.search({
      now: new Date(),
      payload: unvalidated,
    }),
  );
}

export async function terminate(identifier: string): Promise<void> {
  await requireAdmin();

  await unwrapForNextJs(
    AdminMemoWorkflowProvider.terminate({
      now: new Date(),
      payload: { identifier },
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("memos", {});
  await notifyReaderRevalidation([REVALIDATION_TAGS.MEMOS]);
}
