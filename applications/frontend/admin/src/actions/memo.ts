"use server";

import {
  Memo,
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

export async function create(unvalidated: UnvalidatedMemo): Promise<void> {
  await unwrapForNextJs(
    AdminMemoWorkflowProvider.create({
      now: new Date(),
      payload: { unvalidated },
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("memos", {});
}

export async function addEntry(
  unvalidated: UnvalidatedEntry,
  slug: string,
  images: ImageIdentifier[],
): Promise<void> {
  await unwrapForNextJs(
    AdminMemoWorkflowProvider.addEntry({
      now: new Date(),
      payload: { unvalidated, slug, images },
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag(`memo-entries-${slug}`, { expire: 3600 });
}

export async function edit(
  unvalidated: UnvalidatedMemo,
  before: MemoSnapshot,
): Promise<void> {
  await unwrapForNextJs(
    AdminMemoWorkflowProvider.edit({
      now: new Date(),
      payload: { unvalidated, before },
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("memos", {});
}

export async function search(
  unvalidated: UnvalidatedCriteria,
): Promise<Memo[]> {
  return await unwrapForNextJs(
    AdminMemoWorkflowProvider.search({
      now: new Date(),
      payload: unvalidated,
    }),
  );
}

export async function terminate(identifier: string): Promise<void> {
  await unwrapForNextJs(
    AdminMemoWorkflowProvider.terminate({
      now: new Date(),
      payload: { identifier },
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("memos", {});
}
