export async function notifyReaderRevalidation(tags: string[]): Promise<void> {
  const readerUrl = process.env.READER_URL;
  const secret = process.env.REVALIDATION_SECRET;

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
