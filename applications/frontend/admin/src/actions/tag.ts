"use server";

import { cache } from "react";
import { Tag, UnvalidatedCriteria, UnvalidatedTag } from "@shared/domains/attributes/tag";
import { AdminTagWorkflowProvider } from "@/providers/workflows/tag";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { revalidateTag } from "next/cache";
import { requireAdmin } from "@/aspects/auth-guard";

export const find = cache(async (identifier: string): Promise<Tag> => {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminTagWorkflowProvider.ofIdentifiers([identifier]).map((tags) => tags[0]),
  );
});

export const findAllTags = cache(
  async (identifiers: string[]): Promise<Tag[]> => {
    await requireAdmin();
    return await unwrapForNextJs(
      AdminTagWorkflowProvider.ofIdentifiers(identifiers),
    );
  },
);

export const getAllTags = cache(async (): Promise<Tag[]> => {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminTagWorkflowProvider.search({
      payload: {
        name: null,
      },
      now: new Date(),
    }),
  );
});

export const search = cache(
  async (unvalidated: UnvalidatedCriteria): Promise<Tag[]> => {
    await requireAdmin();
    return await unwrapForNextJs(
      AdminTagWorkflowProvider.search({ payload: unvalidated, now: new Date() }),
    );
  },
);

export const ofNames = cache(async (names: string[]): Promise<Tag[]> => {
  await requireAdmin();
  return await unwrapForNextJs(AdminTagWorkflowProvider.ofNames(names));
});

export async function persist(unvalidated: UnvalidatedTag): Promise<void> {
  await unwrapForNextJs(
    AdminTagWorkflowProvider.persist({ payload: unvalidated, now: new Date() }),
  );

  revalidateTag(`tags`, {});
}

export async function terminate(identifier: string): Promise<void> {
  await unwrapForNextJs(
    AdminTagWorkflowProvider.terminate({ payload: identifier, now: new Date() }),
  );

  revalidateTag(`tags`, {});
}
