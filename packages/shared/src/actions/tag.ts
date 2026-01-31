"use server";

import { cache } from "react";
import { Tag, UnvalidatedCriteria } from "@shared/domains/attributes/tag";
import { TagWorkflowProvider } from "@shared/providers/workflows/tag";
import { unwrapForNextJs } from "@shared/components/global/next-error";

export const find = cache(async (identifier: string): Promise<Tag> => {
  return await unwrapForNextJs(
    TagWorkflowProvider.ofIdentifiers([identifier]).map((tags) => tags[0]),
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
