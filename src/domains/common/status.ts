import { z } from "zod/mini";

export const PublishStatus = {
  DRAFT: "draft",
  PUBLISHED: "published",
  ARCHIVED: "archived",
} as const;

export const publishStatusSchema = z.enum(PublishStatus).brand("PublishStatus");

export type PublishStatus = z.infer<typeof publishStatusSchema>;
