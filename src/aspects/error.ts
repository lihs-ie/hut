import z from "zod";

export type DomainError<T> = T;

export type ValidationError = DomainError<{
  field: string;
  description: string;
}>;

export const validationError = (
  field: string,
  description: string
): ValidationError => ({
  field,
  description,
});

export const validationErrors = <T>(
  parser: z.ZodObject,
  candidate: T
): ValidationError[] => {
  const result = parser.safeParse(candidate);

  if (result.success) {
    return [];
  }

  return result.error.issues.map((issue) =>
    validationError(issue.path.join("."), issue.message)
  );
};

export type AggregateNotFoundError<N extends string> = DomainError<{
  name: N;
  identifier: string;
}>;

export const aggregateNotFoundError = <N extends string>(
  name: N,
  identifier: string
): AggregateNotFoundError<N> => ({
  name,
  identifier,
});

export type DuplicationError<N extends string> = DomainError<{
  name: N;
  identifier: string;
}>;

export const duplicationError = <N extends string>(
  name: N,
  identifier: string
): DuplicationError<N> => ({
  name,
  identifier,
});
