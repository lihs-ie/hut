import z from "zod";
import { err, ok, Result } from "@shared/aspects/result";
import { ValidationError, validationError } from "@shared/aspects/error";

export const SUPPORTED_IMAGE_MIME_TYPES = [
  "image/jpeg",
  "image/png",
  "image/webp",
  "image/gif",
] as const;

export type SupportedImageMimeType =
  (typeof SUPPORTED_IMAGE_MIME_TYPES)[number];

export const MAX_IMAGE_SIZE_BEFORE_COMPRESSION = 10 * 1024 * 1024;

export const IMAGE_COMPRESSION_CONFIG = {
  maxWidth: 1200,
  maxHeight: 1200,
  quality: 0.8,
} as const;

export const CONTENT_TYPES = ["article", "memo", "chapter"] as const;
export type ContentType = (typeof CONTENT_TYPES)[number];

export const imageUploadPathSchema = z.string().brand("ImageUploadPath");
export type ImageUploadPath = z.infer<typeof imageUploadPathSchema>;

export const generateImageFileName = (
  originalName: string,
  extension: string,
): string => {
  const timestamp = Date.now();
  const sanitized = originalName
    .replace(/\.[^.]+$/, "")
    .replace(/[^a-zA-Z0-9_-]/g, "_")
    .slice(0, 50);
  return `${timestamp}_${sanitized}.${extension}`;
};

export const generateUploadPath = (
  contentType: ContentType,
  identifiers: { primary: string; secondary?: string },
  fileName: string,
): ImageUploadPath => {
  switch (contentType) {
    case "article":
      return imageUploadPathSchema.parse(
        `articles/${identifiers.primary}/${fileName}`,
      );
    case "memo":
      return imageUploadPathSchema.parse(
        `memos/${identifiers.primary}/${fileName}`,
      );
    case "chapter":
      if (!identifiers.secondary) {
        throw new Error("Chapter requires secondary identifier (chapterId)");
      }
      return imageUploadPathSchema.parse(
        `series/${identifiers.primary}/chapters/${identifiers.secondary}/${fileName}`,
      );
  }
};

export const validateImageFile = (
  file: File,
): Result<File, ValidationError> => {
  if (
    !SUPPORTED_IMAGE_MIME_TYPES.includes(file.type as SupportedImageMimeType)
  ) {
    return err(
      validationError(
        "imageType",
        `サポートされていない画像形式です: ${file.type}。対応形式: JPEG, PNG, WebP, GIF`,
      ),
    );
  }

  if (file.size > MAX_IMAGE_SIZE_BEFORE_COMPRESSION) {
    const sizeMb = Math.round(file.size / 1024 / 1024);
    const maxSizeMb = Math.round(
      MAX_IMAGE_SIZE_BEFORE_COMPRESSION / 1024 / 1024,
    );
    return err(
      validationError(
        "imageSize",
        `ファイルサイズが大きすぎます: ${sizeMb}MB。最大: ${maxSizeMb}MB`,
      ),
    );
  }

  return ok(file);
};

export const getExtensionFromMimeType = (mimeType: string): string => {
  const mimeToExtension: Record<string, string> = {
    "image/jpeg": "jpg",
    "image/png": "png",
    "image/webp": "webp",
    "image/gif": "gif",
  };
  return mimeToExtension[mimeType] ?? "jpg";
};
