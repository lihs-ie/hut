import { after } from "next/server";
import { revalidation } from "@/config/revalidation";

const REVALIDATION_FETCH_TIMEOUT_MILLISECONDS = 5000;

export function notifyReaderRevalidation(tags: string[]): void {
  if (!revalidation) {
    return;
  }

  const readerEndpoint = revalidation.readerEndpoint;
  const secret = revalidation.secret;

  after(async () => {
    try {
      await fetch(`${readerEndpoint}/api/revalidate`, {
        method: "POST",
        headers: {
          "content-type": "application/json",
          "x-revalidation-secret": secret,
        },
        body: JSON.stringify({ tags }),
        signal: AbortSignal.timeout(REVALIDATION_FETCH_TIMEOUT_MILLISECONDS),
      });
    } catch (error: unknown) {
      console.error("Failed to notify reader revalidation:", error);
    }
  });
}
