import { revalidation } from "@/config/revalidation";

export async function notifyReaderRevalidation(tags: string[]): Promise<void> {
  const { readerUrl, secret } = revalidation;

  if (!readerUrl || !secret) {
    return;
  }

  try {
    await fetch(`${readerUrl}/api/revalidate`, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        "x-revalidation-secret": secret,
      },
      body: JSON.stringify({ tags }),
    });
  } catch {
  }
}
