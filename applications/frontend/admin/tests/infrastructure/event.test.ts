import { describe, it, expect, vi, beforeEach } from "vitest";
import type { Topic } from "@google-cloud/pubsub";
import { PubSubEventBroker, mapPubSubError } from "@/infrastructure/event";
import type { Event, EventType } from "@shared/domains/common";
import {
  isUnexpectedError,
  isPermissionDeniedError,
  isUnauthenticatedError,
  isAggregateNotFoundError,
  isResourceExhaustedError,
  isServiceUnavailableError,
} from "@shared/aspects/error";

interface GrpcError {
  readonly code: number;
  readonly details?: string;
  readonly message?: string;
}

const createGrpcError = (code: number, details?: string, message?: string): GrpcError => ({
  code,
  ...(details && { details }),
  ...(message && { message }),
});

const createTestEvent = <T extends EventType, P>(
  type: T,
  payload: P,
  occurredAt = new Date("2024-01-01T00:00:00Z")
): Event<T, P> => ({
  identifier: "test-event-id",
  type,
  payload,
  occurredAt,
});

describe("infrastructure/event", () => {
  describe("mapPubSubError", () => {
    describe("gRPC ステータスコードのマッピング", () => {
      const serviceUnavailableRetryableCodes = [
        { code: 14, name: "UNAVAILABLE" },
        { code: 4, name: "DEADLINE_EXCEEDED" },
        { code: 10, name: "ABORTED" },
      ];

      const serviceUnavailableNonRetryableCodes = [
        { code: 1, name: "CANCELLED" },
      ];

      const unexpectedErrorCodes = [
        { code: 9, name: "FAILED_PRECONDITION" },
        { code: 3, name: "INVALID_ARGUMENT" },
        { code: 11, name: "OUT_OF_RANGE" },
        { code: 12, name: "UNIMPLEMENTED" },
        { code: 13, name: "INTERNAL" },
        { code: 15, name: "DATA_LOSS" },
        { code: 2, name: "UNKNOWN" },
        { code: 999, name: "未知のコード" },
      ];

      it("PERMISSION_DENIED (7) を PermissionDeniedError にマッピングする", () => {
        const result = mapPubSubError(createGrpcError(7, "Permission denied"));

        expect(isPermissionDeniedError(result)).toBe(true);
        expect(result.message).toBe("Permission denied");
      });

      it("UNAUTHENTICATED (16) を UnauthenticatedError にマッピングする", () => {
        const result = mapPubSubError(createGrpcError(16, "Unauthenticated"));

        expect(isUnauthenticatedError(result)).toBe(true);
        expect(result.message).toBe("Unauthenticated");
      });

      it("NOT_FOUND (5) を AggregateNotFoundError にマッピングする", () => {
        const result = mapPubSubError(createGrpcError(5, "Topic not found"));

        expect(isAggregateNotFoundError(result)).toBe(true);
        expect(result.message).toBe("Topic not found");
      });

      it("RESOURCE_EXHAUSTED (8) を ResourceExhaustedError にマッピングする", () => {
        const result = mapPubSubError(createGrpcError(8, "Quota exceeded"));

        expect(isResourceExhaustedError(result)).toBe(true);
        expect(result.message).toBe("Quota exceeded");
      });

      serviceUnavailableRetryableCodes.forEach(({ code, name }) => {
        it(`${name} (${code}) を ServiceUnavailableError (retryable: true) にマッピングする`, () => {
          const result = mapPubSubError(createGrpcError(code, name));

          expect(isServiceUnavailableError(result)).toBe(true);
          if (isServiceUnavailableError(result)) {
            expect(result.retryable).toBe(true);
          }
        });
      });

      serviceUnavailableNonRetryableCodes.forEach(({ code, name }) => {
        it(`${name} (${code}) を ServiceUnavailableError (retryable: false) にマッピングする`, () => {
          const result = mapPubSubError(createGrpcError(code, name));

          expect(isServiceUnavailableError(result)).toBe(true);
          if (isServiceUnavailableError(result)) {
            expect(result.retryable).toBe(false);
          }
        });
      });

      unexpectedErrorCodes.forEach(({ code, name }) => {
        it(`${name} (${code}) を UnexpectedError にマッピングする`, () => {
          const result = mapPubSubError(createGrpcError(code, name));

          expect(isUnexpectedError(result)).toBe(true);
        });
      });
    });

    describe("メッセージの優先順位", () => {
      it("details がある場合は details を使用する", () => {
        const result = mapPubSubError(createGrpcError(7, "Details message", "Message"));

        expect(result.message).toBe("Details message");
      });

      it("details がない場合は message を使用する", () => {
        const result = mapPubSubError(createGrpcError(7, undefined, "Message only"));

        expect(result.message).toBe("Message only");
      });

      it("details と message がない場合はデフォルトメッセージを使用する", () => {
        const result = mapPubSubError({ code: 7 });

        expect(result.message).toBe("PubSub operation failed");
      });
    });

    describe("非 gRPC エラー", () => {
      const nonGrpcErrorCases = [
        { value: new Error("Standard error"), expectedMessage: "Standard error" },
        { value: null, expectedMessage: "Unknown PubSub error" },
        { value: undefined, expectedMessage: "Unknown PubSub error" },
        { value: "String error", expectedMessage: "Unknown PubSub error" },
        { value: { message: "No code" }, expectedMessage: "Unknown PubSub error" },
        { value: { code: "not-a-number" }, expectedMessage: "Unknown PubSub error" },
      ];

      nonGrpcErrorCases.forEach(({ value, expectedMessage }) => {
        it(`${String(value)} を UnexpectedError にマッピングする`, () => {
          const result = mapPubSubError(value);

          expect(isUnexpectedError(result)).toBe(true);
          expect(result.message).toBe(expectedMessage);
        });
      });
    });
  });

  describe("PubSubEventBroker", () => {
    let mockTopic: Topic;
    let mockPublishMessage: ReturnType<typeof vi.fn>;

    const createMockTopic = (publishMessage: ReturnType<typeof vi.fn>): Topic => {
      const topic = Object.create(null) as Topic;
      Object.defineProperty(topic, "publishMessage", {
        value: publishMessage,
        writable: true,
        configurable: true,
      });
      return topic;
    };

    beforeEach(() => {
      vi.clearAllMocks();
      mockPublishMessage = vi.fn().mockResolvedValue("message-id");
      mockTopic = createMockTopic(mockPublishMessage);
    });

    describe("publish", () => {
      it("イベントをシリアライズして発行する", async () => {
        const broker = PubSubEventBroker(mockTopic);
        const event = createTestEvent("article.created", { articleId: "article-123" });

        const result = await broker.publish(event).unwrap();

        expect(result).toBeUndefined();
        expect(mockPublishMessage).toHaveBeenCalledWith({
          data: expect.any(Buffer),
        });
      });

      it("イベントを正しい JSON 形式でシリアライズする", async () => {
        const broker = PubSubEventBroker(mockTopic);
        const event = createTestEvent("article.edited", {
          articleId: "article-456",
          changes: ["title", "content"],
        });

        await broker.publish(event).unwrap();

        const publishCall = mockPublishMessage.mock.calls[0];
        const dataBuffer = (publishCall[0] as { data: Buffer }).data;
        const serialized = JSON.parse(dataBuffer.toString("utf8"));

        expect(serialized.type).toBe("article.edited");
        expect(serialized.payload).toEqual({
          articleId: "article-456",
          changes: ["title", "content"],
        });
      });

      it("発行エラーをマッピングする", async () => {
        mockPublishMessage.mockRejectedValue(createGrpcError(7, "Permission denied for topic"));
        const broker = PubSubEventBroker(mockTopic);
        const event = createTestEvent("article.terminated", { articleId: "article-789" });

        const error = await broker.publish(event).unwrapError();

        expect(isPermissionDeniedError(error)).toBe(true);
      });

      it("ネットワークエラーをハンドリングする", async () => {
        mockPublishMessage.mockRejectedValue(createGrpcError(14, "Service temporarily unavailable"));
        const broker = PubSubEventBroker(mockTopic);
        const event = createTestEvent("article.created", { articleId: "article-network-error" });

        const error = await broker.publish(event).unwrapError();

        expect(isServiceUnavailableError(error)).toBe(true);
      });

      it("空のペイロードでも発行できる", async () => {
        const broker = PubSubEventBroker(mockTopic);
        const event = createTestEvent("memo.created", {});

        const result = await broker.publish(event).unwrap();

        expect(result).toBeUndefined();
        expect(mockPublishMessage).toHaveBeenCalled();
      });

      it("複雑なネストされたペイロードを処理できる", async () => {
        const broker = PubSubEventBroker(mockTopic);
        const event = createTestEvent("memo.edited", {
          user: { id: "user-1", profile: { name: "Test User" } },
          items: ["item1", "item2", "item3"],
        });

        await broker.publish(event).unwrap();

        const publishCall = mockPublishMessage.mock.calls[0];
        const dataBuffer = (publishCall[0] as { data: Buffer }).data;
        const serialized = JSON.parse(dataBuffer.toString("utf8"));

        expect(serialized.payload.user.profile.name).toBe("Test User");
        expect(serialized.payload.items).toHaveLength(3);
      });
    });

    describe("EventBroker インターフェース", () => {
      it("publish メソッドを持つ", () => {
        const broker = PubSubEventBroker(mockTopic);

        expect(typeof broker.publish).toBe("function");
      });
    });
  });
});
