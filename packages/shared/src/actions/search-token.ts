import { unstable_cache } from "next/cache";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { Article } from "@shared/domains/articles";
import { Memo } from "@shared/domains/memo";
import { UnvalidatedCriteria } from "@shared/domains/search-token";
import { Series } from "@shared/domains/series";
import { SearchTokenWorkflowProvider } from "@shared/providers/workflows/search-token";

const searchByTokenInternal = async (
  unvalidated: UnvalidatedCriteria,
): Promise<(Article | Series | Memo)[]> => {
  return await unwrapForNextJs(
    SearchTokenWorkflowProvider.search({
      payload: unvalidated,
      now: new Date(),
    }),
  );
};

export const searchByToken = (
  unvalidated: UnvalidatedCriteria,
): Promise<(Article | Series | Memo)[]> =>
  unstable_cache(
    () => searchByTokenInternal(unvalidated),
    ["search-token", JSON.stringify(unvalidated)],
    { revalidate: 3600, tags: ["search-token"] },
  )();
