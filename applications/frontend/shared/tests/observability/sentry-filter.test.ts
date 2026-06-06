import { describe, it, expect } from "vitest";
import type { ErrorEvent, EventHint } from "@sentry/nextjs";
import { filterSensitiveEvent } from "@shared/observability/sentry-filter";

const isRecord = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === "object" && !Array.isArray(value);

const expectRecord = (value: unknown): Record<string, unknown> => {
  if (!isRecord(value)) {
    throw new Error("Expected object, got " + typeof value);
  }
  return value;
};

const expectArray = (value: unknown): Array<Record<string, unknown>> => {
  if (!Array.isArray(value)) {
    throw new Error("Expected array");
  }
  return value.map((element) => expectRecord(element));
};

describe("observability/sentry-filter", () => {
  describe("filterSensitiveEvent", () => {
    it("request.headers.authorization を [REDACTED] に置換する", () => {
      const event: ErrorEvent = {
        type: undefined,
        request: {
          headers: {
            authorization: "Bearer secret-token",
            "content-type": "application/json",
          },
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});

      expect(filteredEvent).not.toBeNull();
      expect(filteredEvent?.request?.headers?.authorization).toBe("[REDACTED]");
      expect(filteredEvent?.request?.headers?.["content-type"]).toBe(
        "application/json",
      );
    });

    it("request.headers.Authorization（大文字）も [REDACTED] に置換する", () => {
      const event: ErrorEvent = {
        type: undefined,
        request: {
          headers: {
            Authorization: "Bearer secret-token",
          },
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});

      expect(filteredEvent?.request?.headers?.Authorization).toBe("[REDACTED]");
    });

    it("request.headers.cookie / set-cookie も [REDACTED] に置換する", () => {
      const event: ErrorEvent = {
        type: undefined,
        request: {
          headers: {
            cookie: "sessionId=abc123",
            "Set-Cookie": "sessionId=abc123",
            "user-agent": "Mozilla/5.0",
          },
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});

      expect(filteredEvent?.request?.headers?.cookie).toBe("[REDACTED]");
      expect(filteredEvent?.request?.headers?.["Set-Cookie"]).toBe("[REDACTED]");
      expect(filteredEvent?.request?.headers?.["user-agent"]).toBe("Mozilla/5.0");
    });

    it("request.cookies の全値を [REDACTED] に置換する", () => {
      const event: ErrorEvent = {
        type: undefined,
        request: {
          cookies: {
            sessionId: "abc123",
            theme: "dark",
          },
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});

      expect(filteredEvent?.request?.cookies?.sessionId).toBe("[REDACTED]");
      expect(filteredEvent?.request?.cookies?.theme).toBe("[REDACTED]");
    });

    it("request.data 内の email / password を [REDACTED] に置換する", () => {
      const event: ErrorEvent = {
        type: undefined,
        request: {
          data: {
            email: "user@example.com",
            password: "s3cret",
            username: "user",
          },
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});

      expect(filteredEvent?.request?.data).toMatchObject({
        email: "[REDACTED]",
        password: "[REDACTED]",
        username: "user",
      });
    });

    it("request.data 内の token / access_token / refresh_token も [REDACTED] に置換する", () => {
      const event: ErrorEvent = {
        type: undefined,
        request: {
          data: {
            token: "t-1",
            access_token: "at-1",
            refresh_token: "rt-1",
            apiKey: "ak-1",
          },
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});
      const data = expectRecord(filteredEvent?.request?.data);
      expect(data.token).toBe("[REDACTED]");
      expect(data.access_token).toBe("[REDACTED]");
      expect(data.refresh_token).toBe("[REDACTED]");
      expect(data.apiKey).toBe("[REDACTED]");
    });

    it("user.email / user.ip_address を削除する", () => {
      const event: ErrorEvent = {
        type: undefined,
        user: {
          id: "user-1",
          email: "user@example.com",
          ip_address: "192.168.0.1",
          username: "user",
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});

      expect(filteredEvent?.user).toBeDefined();
      expect(filteredEvent?.user?.email).toBeUndefined();
      expect(filteredEvent?.user?.ip_address).toBeUndefined();
      expect(filteredEvent?.user?.id).toBe("user-1");
      expect(filteredEvent?.user?.username).toBe("user");
    });

    it("ネストされたオブジェクト内の email / password / authorization も再帰的にマスクする", () => {
      const event: ErrorEvent = {
        type: undefined,
        request: {
          data: {
            nested: {
              email: "user@example.com",
              password: "s3cret",
              authorization: "Bearer abc",
              deeper: {
                email: "deep@example.com",
              },
            },
          },
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});
      const data = expectRecord(filteredEvent?.request?.data);
      const nested = expectRecord(data.nested);
      expect(nested.email).toBe("[REDACTED]");
      expect(nested.password).toBe("[REDACTED]");
      expect(nested.authorization).toBe("[REDACTED]");
      const deeper = expectRecord(nested.deeper);
      expect(deeper.email).toBe("[REDACTED]");
    });

    it("配列内の email / password も再帰的にマスクする", () => {
      const event: ErrorEvent = {
        type: undefined,
        request: {
          data: {
            items: [
              { email: "a@example.com", value: 1 },
              { email: "b@example.com", value: 2 },
            ],
          },
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});
      const data = expectRecord(filteredEvent?.request?.data);
      const items = expectArray(data.items);
      expect(items[0]).toMatchObject({ email: "[REDACTED]", value: 1 });
      expect(items[1]).toMatchObject({ email: "[REDACTED]", value: 2 });
    });

    it("extra 内の email / password も再帰的にマスクする", () => {
      const event: ErrorEvent = {
        type: undefined,
        extra: {
          formValues: {
            email: "user@example.com",
            password: "s3cret",
            note: "remember me",
          },
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});
      const formValues = expectRecord(filteredEvent?.extra?.formValues);
      expect(formValues.email).toBe("[REDACTED]");
      expect(formValues.password).toBe("[REDACTED]");
      expect(formValues.note).toBe("remember me");
    });

    it("contexts 内の token / apiKey も再帰的にマスクする", () => {
      const event: ErrorEvent = {
        type: undefined,
        contexts: {
          session: {
            token: "t-1",
            state: "active",
          },
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});
      const sessionContext = expectRecord(filteredEvent?.contexts?.session);
      expect(sessionContext.token).toBe("[REDACTED]");
      expect(sessionContext.state).toBe("active");
    });

    it("breadcrumbs[].data 内の email / password も再帰的にマスクする", () => {
      const event: ErrorEvent = {
        type: undefined,
        breadcrumbs: [
          {
            category: "ui.click",
            message: "sign in",
            data: {
              email: "user@example.com",
              source: "header",
            },
          },
        ],
      };

      const filteredEvent = filterSensitiveEvent(event, {});
      expect(filteredEvent?.breadcrumbs?.length).toBe(1);
      const breadcrumbData = expectRecord(filteredEvent?.breadcrumbs?.[0]?.data);
      expect(breadcrumbData.email).toBe("[REDACTED]");
      expect(breadcrumbData.source).toBe("header");
    });

    it("breadcrumb.data が undefined の場合も落ちない", () => {
      const event: ErrorEvent = {
        type: undefined,
        breadcrumbs: [
          {
            category: "ui.click",
            message: "sign in",
          },
        ],
      };

      const filteredEvent = filterSensitiveEvent(event, {});
      expect(filteredEvent?.breadcrumbs?.length).toBe(1);
      expect(filteredEvent?.breadcrumbs?.[0]?.category).toBe("ui.click");
    });

    it("マスク対象外のフィールドは変更されない", () => {
      const event: ErrorEvent = {
        type: undefined,
        request: {
          url: "https://example.com/path",
          method: "POST",
          data: {
            productName: "book",
            price: 1000,
            quantity: 2,
          },
        },
        tags: {
          feature: "cart",
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});

      expect(filteredEvent?.request?.url).toBe("https://example.com/path");
      expect(filteredEvent?.request?.method).toBe("POST");
      expect(filteredEvent?.request?.data).toEqual({
        productName: "book",
        price: 1000,
        quantity: 2,
      });
      expect(filteredEvent?.tags?.feature).toBe("cart");
    });

    it("null 値 / 空配列 / 空オブジェクトはそのまま保持する", () => {
      const event: ErrorEvent = {
        type: undefined,
        request: {
          data: {
            optionalField: null,
            items: [],
            meta: {},
          },
        },
      };

      const filteredEvent = filterSensitiveEvent(event, {});
      const data = expectRecord(filteredEvent?.request?.data);
      expect(data.optionalField).toBeNull();
      expect(data.items).toEqual([]);
      expect(data.meta).toEqual({});
    });

    it("空のイベントもそのまま返す", () => {
      const event: ErrorEvent = { type: undefined };
      const hint: EventHint = {};

      const filteredEvent = filterSensitiveEvent(event, hint);

      expect(filteredEvent).toEqual({ type: undefined });
    });

    it("元のイベントオブジェクトを変更しない", () => {
      const event: ErrorEvent = {
        type: undefined,
        user: {
          email: "user@example.com",
        },
      };

      filterSensitiveEvent(event, {});

      expect(event.user?.email).toBe("user@example.com");
    });
  });
});
