import { revalidation } from "@/config/revalidation";

export async function notifyReaderRevalidation(tags: string[]): Promise<void> {
  if (!revalidation) {
    return;
  }

  try {
    await fetch(`${revalidation.readerEndpoint}/api/revalidate`, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        "x-revalidation-secret": revalidation.secret,
      },
      body: JSON.stringify({ tags }),
    });
  } catch {
  }
}
