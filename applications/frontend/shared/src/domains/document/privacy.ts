import z from "zod";
import { timelineSchema } from "../common";
import { Result } from "@shared/aspects/result";
import { validate, ValidationError } from "@shared/aspects/error";

export const privacyPolicySection = z
  .object({
    headline: z.string().min(1).max(200),
    body: z.string().min(1),
    list: z.array(z.string().min(1)).nullable(),
  })
  .brand("PrivacyPolicyContent");

export type PrivacyPolicySection = z.infer<typeof privacyPolicySection>;

export type UnvalidatedPrivacyPolicySection = {
  headline: string;
  body: string;
  list: null | string[];
};

export const validatePrivacyPolicySection = (
  candidate: UnvalidatedPrivacyPolicySection
): Result<PrivacyPolicySection, ValidationError[]> =>
  validate(privacyPolicySection, candidate);

export const privacyPolicySchema = z
  .object({
    sections: z.array(privacyPolicySection),
    timeline: timelineSchema,
  })
  .brand("PrivacyPolicy");

export type PrivacyPolicy = z.infer<typeof privacyPolicySchema>;

export type UnvalidatedPrivacyPolicy = {
  sections: UnvalidatedPrivacyPolicySection[];
  timeline: {
    createdAt: Date;
    updatedAt: Date;
  };
};

export const validatePrivacyPolicy = (
  candidate: UnvalidatedPrivacyPolicy
): Result<PrivacyPolicy, ValidationError[]> =>
  validate(privacyPolicySchema, candidate);
