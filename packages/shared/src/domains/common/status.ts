import { z } from "zod";

export const publishStatusSchema = z
  .enum(["draft", "published", "archived"])
  .brand("PublishStatus");

export type PublishStatus = z.infer<typeof publishStatusSchema>;

export const PublishStatus = {
  DRAFT: publishStatusSchema.parse("draft"),
  PUBLISHED: publishStatusSchema.parse("published"),
  ARCHIVED: publishStatusSchema.parse("archived"),
} as const;
