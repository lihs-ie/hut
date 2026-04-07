"use server";

import { cache } from "react";
import { Tag, UnvalidatedCriteria } from "@shared/domains/attributes/tag";
import { TagWorkflowProvider } from "@shared/providers/workflows/tag";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { err, ok } from "@shared/aspects/result";
import { aggregateNotFoundError } from "@shared/aspects/error";

export const find = cache(async (identifier: string): Promise<Tag> => {
  return await unwrapForNextJs(
    TagWorkflowProvider.ofIdentifiers([identifier]).andThen((tags: Tag[]) => {
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
    return await unwrapForNextJs(
      TagWorkflowProvider.ofIdentifiers(identifiers),
    );
  },
);

export const getAllTags = cache(async (): Promise<Tag[]> => {
  return await unwrapForNextJs(
    TagWorkflowProvider.search({
      payload: {
        name: null,
      },
      now: new Date(),
    }),
  );
});

export const search = cache(
  async (unvalidated: UnvalidatedCriteria): Promise<Tag[]> => {
    return await unwrapForNextJs(
      TagWorkflowProvider.search({ payload: unvalidated, now: new Date() }),
    );
  },
);

export const ofNames = cache(async (names: string[]): Promise<Tag[]> => {
  return await unwrapForNextJs(TagWorkflowProvider.ofNames(names));
});
