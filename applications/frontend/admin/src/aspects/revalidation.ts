import { revalidation } from "@/config/revalidation";

export function notifyReaderRevalidation(tags: string[]): void {
  if (!revalidation) {
    return;
  }

  fetch(`${revalidation.readerEndpoint}/api/revalidate`, {
    method: "POST",
    headers: {
      "content-type": "application/json",
      "x-revalidation-secret": revalidation.secret,
    },
    body: JSON.stringify({ tags }),
  }).catch((error: unknown) => {
    console.error("Failed to notify reader revalidation:", error);
  });
}
