import z from "zod";
import { err, ok, Result } from "@shared/aspects/result";
import { ValidationError, validationError } from "@shared/aspects/error";

/**
 * サポートする画像MIMEタイプ
 */
export const SUPPORTED_IMAGE_MIME_TYPES = [
  "image/jpeg",
  "image/png",
  "image/webp",
  "image/gif",
] as const;

export type SupportedImageMimeType = (typeof SUPPORTED_IMAGE_MIME_TYPES)[number];

/**
 * 最大ファイルサイズ（圧縮前: 10MB）
 */
export const MAX_IMAGE_SIZE_BEFORE_COMPRESSION = 10 * 1024 * 1024;

/**
 * 圧縮設定
 */
export const IMAGE_COMPRESSION_CONFIG = {
  maxWidth: 1200,
  maxHeight: 1200,
  quality: 0.8,
} as const;

/**
 * コンテンツタイプ（アップロード先の分類）
 */
export const CONTENT_TYPES = ["article", "memo", "chapter"] as const;
export type ContentType = (typeof CONTENT_TYPES)[number];

/**
 * 画像アップロードパスのスキーマ
 */
export const imageUploadPathSchema = z.string().brand("ImageUploadPath");
export type ImageUploadPath = z.infer<typeof imageUploadPathSchema>;

/**
 * ファイル名を生成する
 * @param originalName オリジナルのファイル名
 * @param extension 拡張子
 * @returns 生成されたファイル名
 */
export const generateImageFileName = (
  originalName: string,
  extension: string
): string => {
  const timestamp = Date.now();
  const sanitized = originalName
    .replace(/\.[^.]+$/, "")
    .replace(/[^a-zA-Z0-9_-]/g, "_")
    .slice(0, 50);
  return `${timestamp}_${sanitized}.${extension}`;
};

/**
 * アップロードパスを生成する
 * @param contentType コンテンツタイプ
 * @param identifiers 識別子（articleId, seriesId, chapterIdなど）
 * @param fileName ファイル名
 * @returns アップロードパス
 */
export const generateUploadPath = (
  contentType: ContentType,
  identifiers: { primary: string; secondary?: string },
  fileName: string
): ImageUploadPath => {
  switch (contentType) {
    case "article":
      return imageUploadPathSchema.parse(
        `articles/${identifiers.primary}/${fileName}`
      );
    case "memo":
      return imageUploadPathSchema.parse(
        `memos/${identifiers.primary}/${fileName}`
      );
    case "chapter":
      if (!identifiers.secondary) {
        throw new Error("Chapter requires secondary identifier (chapterId)");
      }
      return imageUploadPathSchema.parse(
        `series/${identifiers.primary}/chapters/${identifiers.secondary}/${fileName}`
      );
  }
};

/**
 * 画像ファイルをバリデートする
 * @param file 検証するファイル
 * @returns バリデーション結果
 */
export const validateImageFile = (
  file: File
): Result<File, ValidationError> => {
  if (
    !SUPPORTED_IMAGE_MIME_TYPES.includes(file.type as SupportedImageMimeType)
  ) {
    return err(
      validationError(
        "imageType",
        `サポートされていない画像形式です: ${file.type}。対応形式: JPEG, PNG, WebP, GIF`
      )
    );
  }

  if (file.size > MAX_IMAGE_SIZE_BEFORE_COMPRESSION) {
    const sizeMb = Math.round(file.size / 1024 / 1024);
    const maxSizeMb = Math.round(MAX_IMAGE_SIZE_BEFORE_COMPRESSION / 1024 / 1024);
    return err(
      validationError(
        "imageSize",
        `ファイルサイズが大きすぎます: ${sizeMb}MB。最大: ${maxSizeMb}MB`
      )
    );
  }

  return ok(file);
};

/**
 * MIMEタイプから拡張子を取得する
 * @param mimeType MIMEタイプ
 * @returns 拡張子
 */
export const getExtensionFromMimeType = (mimeType: string): string => {
  const mimeToExtension: Record<string, string> = {
    "image/jpeg": "jpg",
    "image/png": "png",
    "image/webp": "webp",
    "image/gif": "gif",
  };
  return mimeToExtension[mimeType] ?? "jpg";
};
