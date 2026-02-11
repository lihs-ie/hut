import { after } from "next/server";
import { UnvalidatedCriteria } from "@shared/domains/search-token/common";
import { SearchRecordWorkflowProvider } from "@shared/providers/workflows/analytics/search-record";

export async function recordSearchLog(
  criteria: UnvalidatedCriteria,
  resultCount: number,
): Promise<void> {
  const keyword = criteria.freeWord?.trim();

  if (!keyword) {
    return;
  }

  after(async () => {
    await SearchRecordWorkflowProvider.record({
      now: new Date(),
      payload: {
        keyword,
        resultCount,
        tags: criteria.tags,
        contentType: criteria.type,
      },
    }).tapError((_error) => {});
  });
}
