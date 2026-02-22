"use server";

import { EventBrokerProvider } from "@/providers/domain/event";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import {
  Article,
  ArticleSnapshot,
  UnvalidatedArticle,
  UnvalidatedCriteria,
} from "@shared/domains/articles";
import { AdminArticleWorkflowProvider } from "@/providers/workflows/article";
import { revalidateTag } from "next/cache";

export async function create(unvalidated: UnvalidatedArticle): Promise<void> {
  await unwrapForNextJs(
    AdminArticleWorkflowProvider.create({
      payload: unvalidated,
      now: new Date(),
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag(`articles`, {});
}

export async function edit(
  unvalidated: UnvalidatedArticle,
  before: ArticleSnapshot,
): Promise<void> {
  await unwrapForNextJs(
    AdminArticleWorkflowProvider.edit({
      payload: { unvalidated, before },
      now: new Date(),
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag(`articles`, {});
}

export async function terminate(identifier: string): Promise<void> {
  await unwrapForNextJs(
    AdminArticleWorkflowProvider.terminate({
      payload: { identifier },
      now: new Date(),
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag(`articles`, {});
}

export async function search(
  unvalidated: UnvalidatedCriteria,
): Promise<Article[]> {
  return await unwrapForNextJs(
    AdminArticleWorkflowProvider.search({
      payload: unvalidated,
      now: new Date(),
    }),
  );
}
