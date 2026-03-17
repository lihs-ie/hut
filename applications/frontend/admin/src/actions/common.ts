"use server";

import { unwrapForNextJs, ValidationHttpError } from "@shared/components/global/next-error";
import { AdminImageUploaderProvider } from "../providers/infrastructure/storage";
import { requireAdmin } from "@/aspects/auth-guard";

const ALLOWED_PATH_PREFIXES = ["images/articles/", "images/memos/"] as const;

const ALLOWED_IMAGE_CONTENT_TYPES = [
  "image/jpeg",
  "image/png",
  "image/webp",
  "image/gif",
  "image/svg+xml",
] as const;

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

const validateImageContentType = (file: File | Blob): void => {
  const contentType = file.type;

  if (!contentType) {
    return;
  }

  const isAllowed = ALLOWED_IMAGE_CONTENT_TYPES.includes(
    contentType as (typeof ALLOWED_IMAGE_CONTENT_TYPES)[number],
  );

  if (!isAllowed) {
    throw new ValidationHttpError("無効なファイル形式です", [
      {
        message: `許可されているファイル形式: ${ALLOWED_IMAGE_CONTENT_TYPES.join(", ")}`,
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

  return unwrapForNextJs(
    AdminImageUploaderProvider.firebaseAdmin.upload(file, path),
  );
}
