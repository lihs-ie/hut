import { AwsClient } from "aws4fetch";

/** Secret values supplied through `wrangler secret put`, never through worker configuration. */
export interface R2PresignerSecrets {
  R2_S3_ACCESS_KEY_ID: string;
  R2_S3_SECRET_ACCESS_KEY: string;
  R2_ACCOUNT_IDENTIFIER: string;
}

/** Complete API Worker input for signing; the bucket name is a non-secret Wrangler variable. */
export type R2PresignerEnvironment = R2PresignerSecrets & {
  MEDIA_TMP_UPLOADS_BUCKET_NAME: string;
};

/** Input signed by the API Worker when issuing an upload ticket. */
export interface PresignedPutRequest {
  objectKey: string;
  contentType: string;
  sha256: string;
  expiresInSeconds: number;
}

/** Adapter exposed to generated JSFFI; the request Content-Type is part of the signature. */
export interface MediaApiAdapters {
  presignR2Put(request: PresignedPutRequest): Promise<URL>;
}

function encodedObjectKey(objectKey: string): string {
  const segments = objectKey.split("/");
  if (
    segments.some(
      (segment) =>
        segment.length === 0 || segment === "." || segment === "..",
    )
  ) {
    throw new TypeError(
      "R2 object key must contain only non-empty, non-dot path segments",
    );
  }

  return segments.map((segment) => encodeURIComponent(segment)).join("/");
}

function validatePresignedPutRequest(request: PresignedPutRequest): void {
  if (request.contentType.trim().length === 0) {
    throw new TypeError("Content-Type must not be empty");
  }
  if (!/^[0-9a-f]{64}$/i.test(request.sha256)) {
    throw new TypeError("SHA-256 must contain exactly 64 hexadecimal characters");
  }
  if (
    !Number.isInteger(request.expiresInSeconds) ||
    request.expiresInSeconds < 1 ||
    request.expiresInSeconds > 604_800
  ) {
    throw new RangeError(
      "R2 presigned PUT expiry must be an integer from 1 to 604800 seconds",
    );
  }
}

function sha256HeaderValue(hexDigest: string): string {
  const bytes = new Uint8Array(32);
  for (let index = 0; index < bytes.length; index += 1) {
    bytes[index] = Number.parseInt(hexDigest.slice(index * 2, index * 2 + 2), 16);
  }
  return btoa(String.fromCharCode(...bytes));
}

/** Creates the only TypeScript Cloudflare resource adapter allowed in Media Phase 1. */
export function createMediaApiAdapters(
  environment: R2PresignerEnvironment,
): MediaApiAdapters {
  const client = new AwsClient({
    accessKeyId: environment.R2_S3_ACCESS_KEY_ID,
    secretAccessKey: environment.R2_S3_SECRET_ACCESS_KEY,
    region: "auto",
    service: "s3",
  });

  return {
    async presignR2Put(request: PresignedPutRequest): Promise<URL> {
      validatePresignedPutRequest(request);
      const objectKey = encodedObjectKey(request.objectKey);

      const endpoint = new URL(
        `https://${environment.R2_ACCOUNT_IDENTIFIER}.r2.cloudflare` +
          `storage.com/${environment.MEDIA_TMP_UPLOADS_BUCKET_NAME}/${objectKey}`,
      );

      endpoint.searchParams.set(
        "X-Amz-Expires",
        String(request.expiresInSeconds),
      );

      const signed = await client.sign(
        new Request(endpoint, {
          method: "PUT",
          headers: {
            "content-type": request.contentType,
            "x-amz-checksum-sha256": sha256HeaderValue(request.sha256),
          },
        }),
        { aws: { signQuery: true } },
      );

      return new URL(signed.url);
    },
  };
}
