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

const hasKey = <K extends string>(
  value: object,
  key: K,
): value is { [P in K]: unknown } => key in value;

const convertValue = (value: FirestoreValue): JsonValue => {
  if (hasKey(value, "stringValue") && typeof value.stringValue === "string") {
    return value.stringValue;
  }

  if (hasKey(value, "integerValue")) {
    const raw = value.integerValue;
    if (typeof raw === "number") {
      return raw;
    }
    if (typeof raw === "string") {
      return Number(raw);
    }
    return 0;
  }

  if (hasKey(value, "doubleValue") && typeof value.doubleValue === "number") {
    return value.doubleValue;
  }

  if (hasKey(value, "booleanValue") && typeof value.booleanValue === "boolean") {
    return value.booleanValue;
  }

  if (hasKey(value, "nullValue")) {
    return null;
  }

  if (hasKey(value, "timestampValue") && typeof value.timestampValue === "string") {
    return value.timestampValue;
  }

  if (hasKey(value, "arrayValue") && typeof value.arrayValue === "object" && value.arrayValue !== null) {
    const arrayObject = value.arrayValue;
    if (hasKey(arrayObject, "values") && Array.isArray(arrayObject.values)) {
      return arrayObject.values.map((item) => convertValue(item as FirestoreValue));
    }
    return [];
  }

  if (hasKey(value, "mapValue") && typeof value.mapValue === "object" && value.mapValue !== null) {
    const mapObject = value.mapValue;
    if (
      hasKey(mapObject, "fields") &&
      typeof mapObject.fields === "object" &&
      mapObject.fields !== null
    ) {
      return firestoreFieldsToObject(mapObject.fields as FirestoreFields);
    }
    return {};
  }

  return null;
};

export const firestoreFieldsToObject = (
  fields: FirestoreFields | undefined,
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
