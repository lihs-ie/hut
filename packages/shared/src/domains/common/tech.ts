import { validate, ValidationError } from "@shared/aspects/error";
import { Result } from "@shared/aspects/result";
import z from "zod";

export const TechnologyCategory = {
  FRONTEND: "frontend",
  BACKEND: "backend",
} as const;

export type TechnologyCategory =
  (typeof TechnologyCategory)[keyof typeof TechnologyCategory];

export const technologyCategorySchema = z
  .enum(TechnologyCategory)
  .brand("TechnologyCategory");

export const TechnologyKind = {
  NEXTJS: "nextjs",
  REACT: "react",
  TYPESCRIPT: "typescript",
  GO: "go",
  GIN: "gin",
  JAVA: "java",
  PHP: "php",
  LARAVEL: "laravel",
  RUST: "rust",
} as const;

export type TechnologyKind =
  (typeof TechnologyKind)[keyof typeof TechnologyKind];

export const technologyKindSchema = z
  .enum(TechnologyKind)
  .brand("TechnologyKind");

export const experienceTypeSchema = z
  .enum(["personal", "business", "both"])
  .brand("ExperienceType");

export type ExperienceType = z.infer<typeof experienceTypeSchema>;

export const ExperienceType = {
  PERSONAL: "personal" as ExperienceType,
  BUSINESS: "business" as ExperienceType,
  BOTH: "both" as ExperienceType,
} as const;

export const technologyStackSchema = z
  .object({
    kind: technologyKindSchema,
    from: z.date(),
    continue: z.boolean(),
    type: experienceTypeSchema,
  })
  .brand("TechnologyStack");

export type TechnologyStack = z.infer<typeof technologyStackSchema>;

export type UnvalidatedTechnologyStack = {
  kind: string;
  from: Date;
  continue: boolean;
  type: string;
};

export const validateTechnologyStack = (
  candidate: UnvalidatedTechnologyStack,
): Result<TechnologyStack, ValidationError[]> =>
  validate(technologyStackSchema, candidate);
