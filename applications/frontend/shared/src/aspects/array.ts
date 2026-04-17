import { err, ok, Result } from "@shared/aspects/result";

export const chunk = <T>(
  items: T[],
  size: number,
): Result<T[][], Error> => {
  if (size <= 0) {
    return err(new Error(`chunk size must be greater than 0, got ${size}`));
  }

  if (items.length === 0) {
    return ok([]);
  }

  const result: T[][] = [];

  for (let index = 0; index < items.length; index += size) {
    result.push(items.slice(index, index + size));
  }

  return ok(result);
};
