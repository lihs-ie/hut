import { after } from "next/server";
import { revalidation } from "@/config/revalidation";

const REVALIDATION_FETCH_TIMEOUT_MILLISECONDS = 5000;
const METADATA_SERVER_TIMEOUT_MILLISECONDS = 2000;
const METADATA_SERVER_IDENTITY_URL =
  "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/identity";

const fetchIdentityToken = async (
  audience: string,
): Promise<string | undefined> => {
  try {
    const url = `${METADATA_SERVER_IDENTITY_URL}?audience=${encodeURIComponent(audience)}`;
    const response = await fetch(url, {
      headers: { "Metadata-Flavor": "Google" },
      signal: AbortSignal.timeout(METADATA_SERVER_TIMEOUT_MILLISECONDS),
    });
    if (!response.ok) {
      return undefined;
    }
    const token = (await response.text()).trim();
    return token.length > 0 ? token : undefined;
  } catch {
    return undefined;
  }
};

export function notifyReaderRevalidation(tags: string[]): void {
  if (!revalidation) {
    return;
  }

  const readerEndpoint = revalidation.readerEndpoint;
  const secret = revalidation.secret;

  after(async () => {
    try {
      const identityToken = await fetchIdentityToken(readerEndpoint);
      const headers: Record<string, string> = {
        "content-type": "application/json",
        "x-revalidation-secret": secret,
      };
      if (identityToken !== undefined) {
        headers.authorization = `Bearer ${identityToken}`;
      }
      await fetch(`${readerEndpoint}/api/revalidate`, {
        method: "POST",
        headers,
        body: JSON.stringify({ tags }),
        signal: AbortSignal.timeout(REVALIDATION_FETCH_TIMEOUT_MILLISECONDS),
      });
    } catch (error: unknown) {
      console.error("Failed to notify reader revalidation:", error);
    }
  });
}
