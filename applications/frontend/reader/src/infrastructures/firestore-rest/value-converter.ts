export type FirestoreValue =
  | { stringValue: string }
  | { integerValue: string | number }
  | { doubleValue: number }
  | { booleanValue: boolean }
  | { nullValue: null }
  | { timestampValue: string }
  | { arrayValue: { values?: FirestoreValue[] } }
  | { mapValue: { fields?: FirestoreFields } };

export type FirestoreFields = Record<string, FirestoreValue>;

export type JsonPrimitive = string | number | boolean | null;
export type JsonValue = JsonPrimitive | JsonValue[] | { [key: string]: JsonValue };

const isPlainObject = (
  value: unknown,
): value is Record<string, unknown> => {
  return value !== null && typeof value === "object" && !Array.isArray(value);
};

const convertValue = (value: unknown): JsonValue => {
  if (!isPlainObject(value)) {
    return null;
  }

  if (typeof value.stringValue === "string") {
    return value.stringValue;
  }

  if ("integerValue" in value) {
    const raw = value.integerValue;
    if (typeof raw === "number") {
      return raw;
    }
    if (typeof raw === "string") {
      return Number(raw);
    }
    return 0;
  }

  if (typeof value.doubleValue === "number") {
    return value.doubleValue;
  }

  if (typeof value.booleanValue === "boolean") {
    return value.booleanValue;
  }

  if ("nullValue" in value) {
    return null;
  }

  if (typeof value.timestampValue === "string") {
    return value.timestampValue;
  }

  if (isPlainObject(value.arrayValue)) {
    const items = value.arrayValue.values;
    if (Array.isArray(items)) {
      return items.map((item) => convertValue(item));
    }
    return [];
  }

  if (isPlainObject(value.mapValue)) {
    const nestedFields = value.mapValue.fields;
    if (isPlainObject(nestedFields)) {
      return firestoreFieldsToObject(nestedFields);
    }
    return {};
  }

  return null;
};

export const firestoreFieldsToObject = (
  fields: Record<string, unknown> | undefined,
): Record<string, JsonValue> => {
  if (fields === undefined) {
    return {};
  }

  const result: Record<string, JsonValue> = {};
  for (const [key, value] of Object.entries(fields)) {
    result[key] = convertValue(value);
  }
  return result;
};
