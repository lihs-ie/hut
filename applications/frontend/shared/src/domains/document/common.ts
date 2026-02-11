import z from "zod";
import { privacyPolicySchema, UnvalidatedPrivacyPolicy } from "./privacy";
import { AsyncResult, Result } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  UnexpectedError,
  validate,
  ValidationError,
} from "@shared/aspects/error";

// Aggregate
export const siteDocumentSchema = z
  .object({
    privacy: privacyPolicySchema,
  })
  .brand("SiteDocument");

export type SiteDocument = z.infer<typeof siteDocumentSchema>;

export const validateSiteDocument = (
  candidate: unknown
): Result<SiteDocument, ValidationError[]> =>
  validate(siteDocumentSchema, candidate);

export const updatePrivacy = (
  unvalidated: UnvalidatedPrivacyPolicy
): Result<SiteDocument, ValidationError[]> => {
  const privacyResult = validate(privacyPolicySchema, unvalidated);

  if (privacyResult.isErr) {
    return privacyResult.map(() => null as never);
  }

  return validateSiteDocument({
    privacy: privacyResult.unwrap(),
  });
};

export interface SiteDocumentRepository {
  find: () => AsyncResult<
    SiteDocument,
    AggregateNotFoundError<"SiteDocument"> | UnexpectedError
  >;
  persist: (
    document: SiteDocument
  ) => AsyncResult<
    void,
    AggregateNotFoundError<"SiteDocument"> | UnexpectedError
  >;
}
