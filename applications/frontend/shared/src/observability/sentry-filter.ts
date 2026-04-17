import type { ErrorEvent, EventHint, User } from "@sentry/nextjs";

const SENSITIVE_KEY_NAMES = new Set([
  "email",
  "password",
  "authorization",
  "cookie",
  "set-cookie",
  "x-api-key",
  "api_key",
  "apikey",
  "token",
  "access_token",
  "refresh_token",
  "session",
  "sessionid",
]);
const REDACTED_VALUE = "[REDACTED]";

const isPlainObject = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === "object" && !Array.isArray(value);

const isSensitiveKey = (key: string): boolean =>
  SENSITIVE_KEY_NAMES.has(key.toLowerCase());

const redactSensitiveFields = (value: unknown): unknown => {
  if (Array.isArray(value)) {
    return value.map((element) => redactSensitiveFields(element));
  }

  if (isPlainObject(value)) {
    const redactedEntries: Record<string, unknown> = {};
    for (const [key, fieldValue] of Object.entries(value)) {
      if (isSensitiveKey(key)) {
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
    if (isSensitiveKey(headerName)) {
      redactedHeaders[headerName] = REDACTED_VALUE;
    } else {
      redactedHeaders[headerName] = headerValue;
    }
  }
  return redactedHeaders;
};

const redactCookies = (
  cookies: Record<string, string> | undefined,
): Record<string, string> | undefined => {
  if (cookies === undefined) {
    return undefined;
  }

  const redactedCookies: Record<string, string> = {};
  for (const cookieName of Object.keys(cookies)) {
    redactedCookies[cookieName] = REDACTED_VALUE;
  }
  return redactedCookies;
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
    const redactedCookies = redactCookies(event.request.cookies);
    filteredEvent.request = {
      ...event.request,
      headers: redactedHeaders,
      data: redactedData,
      cookies: redactedCookies,
    };
  }

  if (event.user !== undefined) {
    filteredEvent.user = redactUser(event.user);
  }

  if (event.extra !== undefined) {
    const redactedExtra = redactSensitiveFields(event.extra);
    if (isPlainObject(redactedExtra)) {
      filteredEvent.extra = redactedExtra;
    }
  }

  if (event.contexts !== undefined) {
    const redactedContextEntries: Record<string, Record<string, unknown>> = {};
    for (const [contextName, contextValue] of Object.entries(event.contexts)) {
      if (contextValue === undefined) {
        continue;
      }
      const redacted = redactSensitiveFields(contextValue);
      if (isPlainObject(redacted)) {
        redactedContextEntries[contextName] = redacted;
      }
    }
    filteredEvent.contexts = redactedContextEntries;
  }

  if (event.breadcrumbs !== undefined) {
    filteredEvent.breadcrumbs = event.breadcrumbs.map((breadcrumb) => {
      const redactedData =
        breadcrumb.data === undefined
          ? undefined
          : redactSensitiveFields(breadcrumb.data);
      return {
        ...breadcrumb,
        data: isPlainObject(redactedData) ? redactedData : breadcrumb.data,
      };
    });
  }

  return filteredEvent;
};
