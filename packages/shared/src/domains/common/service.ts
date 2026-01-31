import z from "zod";

export const ExternalServiceType = {
  GITHUB: "gitHub",
  X: "x",
} as const;

export type ExternalServiceType =
  (typeof ExternalServiceType)[keyof typeof ExternalServiceType];

export const externalServiceTypeSchema = z
  .enum(ExternalServiceType)
  .brand("ExternalService");

export const externalServiceSchema = z
  .object({
    type: externalServiceTypeSchema,
    user: z.string().min(1).max(50),
  })
  .brand("ExternalService");

export type ExternalService = z.infer<typeof externalServiceSchema>;
