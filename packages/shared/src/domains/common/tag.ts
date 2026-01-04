import z from "zod";

export const Tag = {
  NEXT_JS: "next.js",
  REACT: "react",
  TYPESCRIPT: "typescript",
  RUST: "rust",
  OTHER: "other",
} as const;

export const tagSchema = z.enum(Tag).brand("Tag");

export type Tag = z.infer<typeof tagSchema>;
