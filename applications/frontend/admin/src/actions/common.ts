"use server";

import { unwrapForNextJs, ValidationHttpError } from "@shared/components/global/next-error";
import { AdminImageUploaderProvider } from "../providers/infrastructure/storage";
import { requireAdmin } from "@/aspects/auth-guard";

const ALLOWED_PATH_PREFIXES = [
  "images/articles/",
  "images/memos/",
  "images/chapters/",
  "images/series/",
] as const;

const ALLOWED_IMAGE_CONTENT_TYPES = [
  "image/jpeg",
  "image/png",
  "image/webp",
  "image/gif",
] as const;

const MAX_IMAGE_SIZE_BYTES = 10 * 1024 * 1024;

const validateUploadPath = (path: string): void => {
  if (path.includes("..") || path.startsWith("./")) {
    throw new ValidationHttpError("無効なパスです", [
      { message: "パスにディレクトリトラバーサル文字は使用できません" },
    ]);
  }

  const hasAllowedPrefix = ALLOWED_PATH_PREFIXES.some((prefix) =>
    path.startsWith(prefix),
  );

  if (!hasAllowedPrefix) {
    throw new ValidationHttpError("無効なパスです", [
      {
        message: `パスは ${ALLOWED_PATH_PREFIXES.join(", ")} で始まる必要があります`,
      },
    ]);
  }
};

const isAllowedImageContentType = (
  contentType: string,
): contentType is (typeof ALLOWED_IMAGE_CONTENT_TYPES)[number] =>
  ALLOWED_IMAGE_CONTENT_TYPES.some((allowed) => allowed === contentType);

const validateImageContentType = (file: File | Blob): void => {
  const contentType = file.type;

  if (!contentType || !isAllowedImageContentType(contentType)) {
    throw new ValidationHttpError("無効なファイル形式です", [
      {
        message: `許可されているファイル形式: ${ALLOWED_IMAGE_CONTENT_TYPES.join(", ")}`,
      },
    ]);
  }
};

const validateImageFileSize = (file: File | Blob): void => {
  if (file.size > MAX_IMAGE_SIZE_BYTES) {
    throw new ValidationHttpError("ファイルサイズが大きすぎます", [
      {
        message: `ファイルサイズは${MAX_IMAGE_SIZE_BYTES / (1024 * 1024)}MB以下にしてください`,
      },
    ]);
  }
};

export async function uploadImage(
  file: File | Blob,
  path: string,
): Promise<string> {
  await requireAdmin();

  validateUploadPath(path);
  validateImageContentType(file);
  validateImageFileSize(file);

  return unwrapForNextJs(
    AdminImageUploaderProvider.firebaseAdmin.upload(file, path),
  );
}
