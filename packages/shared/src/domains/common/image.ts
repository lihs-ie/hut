import { UnexpectedError } from "@shared/aspects/error";
import { AsyncResult } from "@shared/aspects/result";
import z from "zod";

export const imageSchema = z.url().brand("Image");

export type Image = z.infer<typeof imageSchema>;

export interface ImageUploader {
  upload: <T, R>(image: T, path: string) => AsyncResult<R, UnexpectedError>;
}
