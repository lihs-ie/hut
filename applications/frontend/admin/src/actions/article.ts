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
import { requireAdmin } from "@/aspects/auth-guard";
import { notifyReaderRevalidation } from "@/aspects/revalidation";
import { REVALIDATION_TAGS } from "@shared/config/revalidation";

export async function create(unvalidated: UnvalidatedArticle): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(
    AdminArticleWorkflowProvider.create({
      payload: unvalidated,
      now: new Date(),
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("articles");
  notifyReaderRevalidation([REVALIDATION_TAGS.ARTICLES]);
}

export async function edit(
  unvalidated: UnvalidatedArticle,
  before: ArticleSnapshot,
): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(
    AdminArticleWorkflowProvider.edit({
      payload: { unvalidated, before },
      now: new Date(),
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("articles");
  notifyReaderRevalidation([REVALIDATION_TAGS.ARTICLES]);
}

export async function terminate(identifier: string): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(
    AdminArticleWorkflowProvider.terminate({
      payload: { identifier },
      now: new Date(),
    }).andThen(EventBrokerProvider.pubSub.publish),
  );

  revalidateTag("articles");
  notifyReaderRevalidation([REVALIDATION_TAGS.ARTICLES]);
}

export async function findBySlug(slug: string): Promise<Article> {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminArticleWorkflowProvider.findBySlug({ payload: { slug }, now: new Date() }),
  );
}

export async function search(
  unvalidated: UnvalidatedCriteria,
): Promise<Article[]> {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminArticleWorkflowProvider.search({
      payload: unvalidated,
      now: new Date(),
    }),
  );
}
