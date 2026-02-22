import type { RemotePattern } from "next/dist/shared/lib/image-config";

const SUPPORTED_PROTOCOLS = ["http", "https"] as const;

type SupportedProtocol = (typeof SUPPORTED_PROTOCOLS)[number];

const isSupportedProtocol = (value: string): value is SupportedProtocol =>
  (SUPPORTED_PROTOCOLS as ReadonlyArray<string>).includes(value);

export const parseImageRemotePatterns = (
  value: string | undefined
): ReadonlyArray<RemotePattern> => {
  if (!value || value.trim() === "") {
    return [];
  }

  return value
    .split(",")
    .map((entry) => entry.trim())
    .filter((entry) => entry.length > 0)
    .map((entry): RemotePattern => {
      if (entry.includes("*")) {
        throw new Error(
          `Wildcard patterns are not allowed in IMAGE_REMOTE_PATTERNS: "${entry}"`
        );
      }

      let url: URL;

      try {
        url = new URL(entry);
      } catch {
        throw new Error(
          `Invalid URL format in IMAGE_REMOTE_PATTERNS: "${entry}"`
        );
      }

      const protocol = url.protocol.replace(":", "");

      if (!isSupportedProtocol(protocol)) {
        throw new Error(
          `Unsupported protocol "${protocol}" in IMAGE_REMOTE_PATTERNS: "${entry}"`
        );
      }

      return {
        protocol,
        hostname: url.hostname,
      };
    });
};
