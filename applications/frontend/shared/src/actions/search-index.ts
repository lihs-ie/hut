import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Article } from "@shared/domains/articles";
import { Memo } from "@shared/domains/memo";
import { UnvalidatedCriteria } from "@shared/domains/search-index";
import { Series } from "@shared/domains/series";
import { SearchIndexWorkflowProvider } from "@shared/providers/workflows/search-index";

export async function search(
  unvalidated: UnvalidatedCriteria,
): Promise<(Article | Series | Memo)[]> {
  return await unwrapForNextJs(
    SearchIndexWorkflowProvider.search({
      payload: unvalidated,
      now: new Date(),
    }),
  );
}
