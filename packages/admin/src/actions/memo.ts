"use server";

import {
  Memo,
  MemoSnapshot,
  UnvalidatedCriteria,
  UnvalidatedEntry,
  UnvalidatedMemo,
} from "@shared/domains/memo";
import { MemoWorkflowProvider } from "@shared/providers/workflows/memo";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { revalidateTag } from "next/cache";
import { EventBrokerProvider } from "@/providers/domain/event";

export async function create(unvalidated: UnvalidatedMemo): Promise<void> {
  await unwrapForNextJs(
    MemoWorkflowProvider.create({
      now: new Date(),
      payload: { unvalidated },
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("memos", {});
}

export async function addEntry(
  unvalidated: UnvalidatedEntry,
  slug: string,
): Promise<void> {
  await unwrapForNextJs(
    MemoWorkflowProvider.addEntry({
      now: new Date(),
      payload: { unvalidated, slug },
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag(`memo-entries-${slug}`, { expire: 3600 });
}

export async function edit(
  unvalidated: UnvalidatedMemo,
  before: MemoSnapshot,
): Promise<void> {
  await unwrapForNextJs(
    MemoWorkflowProvider.edit({
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
    MemoWorkflowProvider.search({
      now: new Date(),
      payload: unvalidated,
    }),
  );
}

export async function terminate(identifier: string): Promise<void> {
  await unwrapForNextJs(
    MemoWorkflowProvider.terminate({
      now: new Date(),
      payload: { identifier },
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("memos", {});
}
