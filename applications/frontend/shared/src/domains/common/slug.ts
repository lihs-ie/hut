import { validationError, ValidationError } from "@shared/aspects/error";
import { err, ok, Result } from "@shared/aspects/result";
import z from "zod";

export const slugSchema = z
  .string()
  .min(1)
  .max(100)
  .regex(/^[a-z0-9-]+$/, {
    message: `Slug can only contain lowercase letters, numbers, and hyphens, and cannot start or end with a hyphen`,
  })
  .brand("Slug");

export type Slug = z.infer<typeof slugSchema>;

export const validateSlug: ValidateSlug = (
  candidate: unknown
): Result<Slug, ValidationError> => {
  const result = slugSchema.safeParse(candidate);

  if (!result.success) {
    return err(validationError("Slug", `Invalid Slug format: ${candidate}`));
  }

  return ok(result.data);
};

export type ValidateSlug = (slug: string) => Result<Slug, ValidationError>;
