"use server";

import { unstable_cache } from "next/cache";
import { unwrapForNextJs } from "@shared/components/global/next-error";
import { DocumentWorkflowProvider } from "@shared/providers/workflows/document";

export const getPrivacyPolicy = unstable_cache(
  async () => {
    return await unwrapForNextJs(
      DocumentWorkflowProvider.GetPrivacyPolicy({
        payload: null,
        now: new Date(),
      }),
    );
  },
  ["privacy-policy"],
  { revalidate: 3600, tags: ["privacy-policy"] },
);
