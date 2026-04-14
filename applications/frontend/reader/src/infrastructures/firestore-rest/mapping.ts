import type { JsonValue } from "./value-converter";

export const toTimeline = (
  value: JsonValue,
): { createdAt: Date; updatedAt: Date } | null => {
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    return null;
  }
  const record = value as Record<string, JsonValue>;
  const createdAt = record.createdAt;
  const updatedAt = record.updatedAt;
  if (typeof createdAt !== "string" || typeof updatedAt !== "string") {
    return null;
  }
  return {
    createdAt: new Date(createdAt),
    updatedAt: new Date(updatedAt),
  };
};

export const toStringArray = (value: JsonValue): string[] => {
  if (!Array.isArray(value)) {
    return [];
  }
  const result: string[] = [];
  for (const item of value) {
    if (typeof item === "string") {
      result.push(item);
    }
  }
  return result;
};

export const toOptionalString = (value: JsonValue): string | null => {
  if (typeof value === "string") {
    return value;
  }
  return null;
};

export const describeError = (error: unknown): string => {
  if (error instanceof Error) {
    return error.message;
  }
  return String(error);
};
