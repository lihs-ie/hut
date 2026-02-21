import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  validate,
  validationError,
  ValidationError,
} from "@shared/aspects/error";
import { AsyncResult, err, ok, Result } from "@shared/aspects/result";
import z from "zod";
import { ContentType, contentTypeSchema } from "../search-index";
import { articleIdentifierSchema } from "../articles";
import { memoIdentifierSchema } from "../memo";
import { ImageIdentifier, imageIdentifierSchema } from "./identifier";

export type { ImageIdentifier } from "./identifier";
export { imageIdentifierSchema } from "./identifier";

export const imageTypeSchema = z
  .enum(["png", "jpeg", "gif", "webp"])
  .brand("ImageType");

export type ImageType = z.infer<typeof imageTypeSchema>;

export const ImageType = {
  PNG: "png" as ImageType,
  JPEG: "jpeg" as ImageType,
  GIF: "gif" as ImageType,
  WEBP: "webp" as ImageType,
} as const;

export const imageURLSchema = z.url().brand("ImageURL");

export type ImageURL = z.infer<typeof imageURLSchema>;

export const uploadStatusSchema = z
  .enum(["pending", "completed", "failed"])
  .brand("ImageStatus");

export type UploadStatus = z.infer<typeof uploadStatusSchema>;

export const UploadStatus = {
  PENDING: "pending" as UploadStatus,
  COMPLETED: "completed" as UploadStatus,
  FAILED: "failed" as UploadStatus,
} as const;

export const imageSchema = z
  .object({
    identifier: imageIdentifierSchema,
    type: imageTypeSchema,
    url: imageURLSchema.nullable(),
    uploadStatus: uploadStatusSchema,
    reference: articleIdentifierSchema.or(memoIdentifierSchema), // TODO: seriesIdentifierSchemaを追加
    content: contentTypeSchema,
  })
  .refine(
    (data) => {
      if (data.uploadStatus === UploadStatus.COMPLETED) {
        return data.url !== null;
      } else {
        return data.url === null;
      }
    },
    { error: "URL must be set if and only if uploadStatus is 'completed'" },
  )
  .brand("Image");

export type Image = z.infer<typeof imageSchema>;

export type UnvalidatedImage = {
  identifier: string;
  type: string;
  uploadStatus: string;
  url: string | null;
  reference: string;
  content: string;
};

export const validateImage = (
  candidate: UnvalidatedImage,
): Result<Image, ValidationError[]> => validate(imageSchema, candidate);

export const uploaded = (image: Image, url: ImageURL): Image => ({
  ...image,
  url,
  uploadStatus: UploadStatus.COMPLETED,
});

export const generateUploadPath = (
  image: Image,
): Result<string, ValidationError> => {
  switch (image.content) {
    case ContentType.ARTICLE:
    case ContentType.MEMO:
    case ContentType.SERIES:
      return ok(
        `${image.content}s/${image.reference}/${image.identifier}.${image.type}`,
      );
    default:
      return err(
        validationError(
          "content",
          `Cannot generate upload path for content type: ${image.content}`,
        ),
      );
  }
};

export const criteriaSchema = z
  .object({
    type: imageTypeSchema.optional(),
    uploadStatus: uploadStatusSchema.optional(),
    reference: articleIdentifierSchema.or(memoIdentifierSchema).optional(), // TODO: seriesIdentifierSchemaを追加
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export type UnvalidatedCriteria = {
  type?: string;
  uploadStatus?: string;
  reference?: string;
};

export interface ImageRepository {
  find: (
    identifier: ImageIdentifier,
  ) => AsyncResult<Image, AggregateNotFoundError<"Image"> | UnexpectedError>;
  persist: (
    image: Image,
  ) => AsyncResult<void, UnexpectedError | DuplicationError<"Image">>;
  search: (criteria: Criteria) => AsyncResult<Image[], UnexpectedError>;
  terminate: (
    identifier: ImageIdentifier,
  ) => AsyncResult<void, UnexpectedError | AggregateNotFoundError<"Image">>;
}
