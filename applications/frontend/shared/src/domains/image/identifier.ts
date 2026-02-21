import z from "zod";

export const imageIdentifierSchema = z.ulid().brand("ImageIdentifier");

export type ImageIdentifier = z.infer<typeof imageIdentifierSchema>;
