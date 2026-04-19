"use server";

import { SearchReferenceIdentifier } from "@shared/domains/search-token/reference";

export async function incrementViewCount(
  _identifier: SearchReferenceIdentifier,
): Promise<void> {
  return;
}
