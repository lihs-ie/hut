import z from "zod";
import { Event, timelineSchema } from "../common";
import {
  AggregateNotFoundError,
  DuplicationError,
  UnexpectedError,
  validate,
  ValidationError,
  validationError,
} from "@shared/aspects/error";
import { AsyncResult, Result, ok, err } from "@shared/aspects/result";
import { imageSchema } from "../common/image";

export const tagIdentifierSchema = z.ulid().brand("TagIdentifier");

export type TagIdentifier = z.infer<typeof tagIdentifierSchema>;

export const tagNameSchema = z.string().min(1).max(20).brand("TagName");

export type TagName = z.infer<typeof tagNameSchema>;

export const tagSchema = z
  .object({
    identifier: tagIdentifierSchema,
    name: tagNameSchema,
    logo: imageSchema,
    timeline: timelineSchema,
  })
  .brand("Tag");

export type Tag = z.infer<typeof tagSchema>;

export type UnvalidatedTag = {
  identifier: string;
  name: string;
  logo: string;
  timeline: {
    createdAt: Date;
    updatedAt: Date;
  };
};

export const validateTag = (
  candidate: UnvalidatedTag,
): Result<Tag, ValidationError[]> => validate(tagSchema, candidate);

export const criteriaSchema = z
  .object({
    name: z.string().max(20).nullable(),
  })
  .brand("Criteria");

export type Criteria = z.infer<typeof criteriaSchema>;

export type UnvalidatedCriteria = {
  name: string | null;
};

export const validateCriteria = (
  candidate: UnvalidatedCriteria,
): Result<Criteria, ValidationError[]> => validate(criteriaSchema, candidate);

export const validateTagIdentifier = (
  identifier: string,
): Result<TagIdentifier, ValidationError> => {
  const result = tagIdentifierSchema.safeParse(identifier);
  if (result.success) {
    return ok(result.data);
  }
  return err(
    validationError(
      "identifier",
      result.error.issues[0]?.message ?? "Invalid identifier",
    ),
  );
};

export const validateTagIdentifiers = (
  identifiers: string[],
): Result<TagIdentifier[], ValidationError[]> => {
  const results = identifiers.map((identifier) =>
    tagIdentifierSchema.safeParse(identifier),
  );
  const errors = results
    .map((result, index) => {
      if (!result.success) {
        return validationError(
          `identifiers[${index}]`,
          result.error.issues[0]?.message ?? "Invalid identifier",
        );
      }
      return null;
    })
    .filter((error): error is ValidationError => error !== null);

  if (errors.length > 0) {
    return err(errors);
  }

  return ok(results.map((result) => (result as { data: TagIdentifier }).data));
};

export const validateTagName = (
  name: string,
): Result<TagName, ValidationError> => {
  const result = tagNameSchema.safeParse(name);
  if (result.success) {
    return ok(result.data);
  }
  return err(
    validationError("name", result.error.issues[0]?.message ?? "Invalid name"),
  );
};

export const validateTagNames = (
  names: string[],
): Result<TagName[], ValidationError[]> => {
  const results = z.array(tagNameSchema).safeParse(names);
  if (results.success) {
    return ok(results.data);
  }

  return err(
    results.error.issues.map((issue) =>
      validationError(issue.path.join("."), issue.message),
    ),
  );
};

export interface TagRepository {
  find(
    identifier: TagIdentifier,
  ): AsyncResult<Tag, AggregateNotFoundError<"Tag"> | UnexpectedError>;
  search(criteria: Criteria): AsyncResult<Tag[], UnexpectedError>;
  ofIdentifiers(
    identifiers: TagIdentifier[],
  ): AsyncResult<Tag[], AggregateNotFoundError<"Tag"> | UnexpectedError>;
  ofNames(names: TagName[]): AsyncResult<Tag[], UnexpectedError>;
  persist(
    tag: Tag,
  ): AsyncResult<void, UnexpectedError | DuplicationError<"Tag">>;
  terminate(
    identifier: TagIdentifier,
  ): AsyncResult<void, AggregateNotFoundError<"Tag"> | UnexpectedError>;
}

export type TagPersistedEvent = Event<
  "tag.persisted",
  { identifier: TagIdentifier }
>;

export type TagTerminatedEvent = Event<
  "tag.terminated",
  { identifier: TagIdentifier }
>;
