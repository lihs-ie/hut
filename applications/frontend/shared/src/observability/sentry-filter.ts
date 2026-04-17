import type { ErrorEvent, EventHint, User } from "@sentry/nextjs";

const SENSITIVE_KEY_NAMES = new Set(["email", "password", "authorization"]);
const REDACTED_VALUE = "[REDACTED]";

const isPlainObject = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === "object" && !Array.isArray(value);

const redactSensitiveFields = (value: unknown): unknown => {
  if (Array.isArray(value)) {
    return value.map((element) => redactSensitiveFields(element));
  }

  if (isPlainObject(value)) {
    const redactedEntries: Record<string, unknown> = {};
    for (const [key, fieldValue] of Object.entries(value)) {
      if (SENSITIVE_KEY_NAMES.has(key.toLowerCase())) {
        redactedEntries[key] = REDACTED_VALUE;
      } else {
        redactedEntries[key] = redactSensitiveFields(fieldValue);
      }
    }
    return redactedEntries;
  }

  return value;
};

const redactHeaders = (
  headers: Record<string, string> | undefined,
): Record<string, string> | undefined => {
  if (headers === undefined) {
    return undefined;
  }

  const redactedHeaders: Record<string, string> = {};
  for (const [headerName, headerValue] of Object.entries(headers)) {
    if (headerName.toLowerCase() === "authorization") {
      redactedHeaders[headerName] = REDACTED_VALUE;
    } else {
      redactedHeaders[headerName] = headerValue;
    }
  }
  return redactedHeaders;
};

const redactUser = (user: User | undefined): User | undefined => {
  if (user === undefined) {
    return undefined;
  }

  const filtered: User = {};
  for (const [key, value] of Object.entries(user)) {
    if (key === "email" || key === "ip_address") {
      continue;
    }
    filtered[key] = value;
  }
  return filtered;
};

export const filterSensitiveEvent = (
  event: ErrorEvent,
  _hint: EventHint,
): ErrorEvent | null => {
  void _hint;
  const filteredEvent: ErrorEvent = { ...event };

  if (event.request !== undefined) {
    const redactedHeaders = redactHeaders(event.request.headers);
    const redactedData = redactSensitiveFields(event.request.data);
    filteredEvent.request = {
      ...event.request,
      headers: redactedHeaders,
      data: redactedData,
    };
  }

  if (event.user !== undefined) {
    filteredEvent.user = redactUser(event.user);
  }

  return filteredEvent;
};
