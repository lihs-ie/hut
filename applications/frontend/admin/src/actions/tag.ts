"use server";

import { cache } from "react";
import { Tag, UnvalidatedCriteria, UnvalidatedTag } from "@shared/domains/attributes/tag";
import { AdminTagWorkflowProvider } from "@/providers/workflows/tag";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { revalidateTag } from "next/cache";
import { requireAdmin } from "@/aspects/auth-guard";
import { err, ok } from "@shared/aspects/result";
import { aggregateNotFoundError } from "@shared/aspects/error";

export const find = cache(async (identifier: string): Promise<Tag> => {
  await requireAdmin();
  return await unwrapForNextJs(
    AdminTagWorkflowProvider.ofIdentifiers([identifier]).andThen((tags: Tag[]) => {
      const tag = tags[0];
      if (tag === undefined) {
        return err(aggregateNotFoundError("Tag", `Tag not found: ${identifier}`));
      }
      return ok(tag);
    }),
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
  await requireAdmin();
  await unwrapForNextJs(
    AdminTagWorkflowProvider.persist({ payload: unvalidated, now: new Date() }),
  );

  revalidateTag(`tags`, {});
}

export async function terminate(identifier: string): Promise<void> {
  await requireAdmin();
  await unwrapForNextJs(
    AdminTagWorkflowProvider.terminate({ payload: identifier, now: new Date() }),
  );

  revalidateTag(`tags`, {});
}
