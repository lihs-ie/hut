import { unexpectedError, UnexpectedError } from "@shared/aspects/error";
import { AsyncResult, fromPromise } from "@shared/aspects/result";
import { ImageUploader } from "@shared/domains/common/image";
import { getDownloadURL } from "firebase-admin/storage";
import type { Bucket } from "@google-cloud/storage";

type BlobLike = {
  arrayBuffer: () => Promise<ArrayBuffer>;
  type: string;
};

const isBlobLike = (value: unknown): value is BlobLike =>
  value !== null &&
  typeof value === "object" &&
  "arrayBuffer" in value &&
  typeof value.arrayBuffer === "function" &&
  "type" in value;

const isDataUrl = (value: unknown): value is string =>
  typeof value === "string" && value.startsWith("data:");

const parseDataUrl = (
  dataUrl: string,
): { buffer: Buffer; contentType: string } => {
  const match = dataUrl.match(/^data:([^;]+);base64,(.+)$/);
  if (!match) {
    throw new Error("Invalid data URL format");
  }
  return {
    contentType: match[1],
    buffer: Buffer.from(match[2], "base64"),
  };
};

const mapStorageError = (error: unknown): UnexpectedError => {
  const message = error instanceof Error ? error.message : String(error);
  return unexpectedError(message, error);
};

export const FirebaseAdminStorageImageUploader = (
  bucket: Bucket,
): ImageUploader => {
  const upload: ImageUploader["upload"] = <T, R>(
    image: T,
    path: string,
  ): AsyncResult<R, UnexpectedError> =>
    fromPromise(
      (async () => {
        let buffer: Buffer;
        let contentType: string;

        if (isBlobLike(image)) {
          const arrayBuffer = await image.arrayBuffer();
          buffer = Buffer.from(arrayBuffer);
          contentType = image.type || "application/octet-stream";
        } else if (isDataUrl(image)) {
          const parsed = parseDataUrl(image);
          buffer = parsed.buffer;
          contentType = parsed.contentType;
        } else {
          throw new Error("Unsupported image input type");
        }

        const file = bucket.file(path);
        await file.save(buffer, { contentType });

        const downloadUrl = await getDownloadURL(file);
        return downloadUrl as R;
      })(),
      mapStorageError,
    );

  return { upload };
};
