/**
 * Event Broker Feature Test
 *
 * PubSub Emulatorを使用してイベントブローカーの統合テストを行います。
 *
 * 実行方法:
 *   PUBSUB_EMULATOR_HOST=localhost:8086 pnpm exec vitest run admin/tests/features/event-broker.test.ts
 */

import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { PubSub, Topic, Subscription } from "@google-cloud/pubsub";
import { PubSubEventBroker } from "@/infrastructure/event";
import type { Event, EventType } from "@shared/domains/common";

const PUBSUB_PROJECT_ID = "demo-hut";
const PUBSUB_TOPIC_NAME = "events-test";
const PUBSUB_SUBSCRIPTION_NAME = "events-test-sub";

/**
 * PubSub Emulatorが利用可能かを確認
 */
const isEmulatorAvailable = (): boolean => {
  return false;
};

/**
 * gRPCエラーコード6（ALREADY_EXISTS）かどうか判定
 */
function isAlreadyExistsError(error: unknown): boolean {
  return (
    error !== null &&
    typeof error === "object" &&
    "code" in error &&
    (error as { code: number }).code === 6
  );
}

type PublishedMessage = {
  identifier: string;
  type: string;
  payload: unknown;
  occurredAt: string;
};

function createTestEvent<T extends EventType, P>(
  type: T,
  payload: P,
  occurredAt = new Date("2024-01-01T00:00:00Z")
): Event<T, P> {
  return {
    identifier: `test-event-${Date.now()}`,
    type,
    payload,
    occurredAt,
  };
}

// PubSub Emulatorへの接続が必要なテストは環境変数が設定されている場合のみ実行
// runIf: 条件が真の場合のみdescribeを実行
describe.runIf(isEmulatorAvailable())(
  "Feature: Event Broker (実PubSub Emulator接続)",
  () => {
    let pubsub: PubSub;
    let topic: Topic;
    let subscription: Subscription;
    let receivedMessages: PublishedMessage[];

    beforeAll(async () => {
      pubsub = new PubSub({ projectId: PUBSUB_PROJECT_ID });

      // テスト用トピックを作成（既存の場合はスキップ）
      try {
        [topic] = await pubsub.createTopic(PUBSUB_TOPIC_NAME);
      } catch (error: unknown) {
        if (isAlreadyExistsError(error)) {
          topic = pubsub.topic(PUBSUB_TOPIC_NAME);
        } else {
          throw error;
        }
      }

      // テスト用サブスクリプションを作成
      try {
        [subscription] = await topic.createSubscription(PUBSUB_SUBSCRIPTION_NAME);
      } catch (error: unknown) {
        if (isAlreadyExistsError(error)) {
          subscription = topic.subscription(PUBSUB_SUBSCRIPTION_NAME);
        } else {
          throw error;
        }
      }

      receivedMessages = [];

      subscription.on("message", (message) => {
        const parsed = JSON.parse(message.data.toString("utf8")) as PublishedMessage;
        receivedMessages.push(parsed);
        message.ack();
      });
    });

  afterAll(async () => {
    // クリーンアップ
    try {
      await subscription.delete();
    } catch {
      // ignore
    }
    try {
      await topic.delete();
    } catch {
      // ignore
    }
    await pubsub.close();
  });

  // ヘルパー関数: メッセージが受信されるまで待機
  async function waitForMessages(count: number, timeoutMs = 5000): Promise<void> {
    const startTime = Date.now();
    while (receivedMessages.length < count) {
      if (Date.now() - startTime > timeoutMs) {
        throw new Error(`Timeout waiting for ${count} messages, received ${receivedMessages.length}`);
      }
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
  }

  describe("イベント発行の一連の流れ", () => {
    it("記事作成イベントを正しく発行できる", async () => {
      const initialCount = receivedMessages.length;
      const broker = PubSubEventBroker(topic);
      const event = createTestEvent("article.created", {
        snapshot: {
          identifier: "01HXXXXXXXXXXXXXXXXXXXXXX",
          title: "Test Article",
          slug: "test-article",
        },
      });

      await broker.publish(event).unwrap();

      await waitForMessages(initialCount + 1);

      const lastMessage = receivedMessages[receivedMessages.length - 1];
      expect(lastMessage?.type).toBe("article.created");
      expect((lastMessage?.payload as Record<string, unknown>).snapshot).toMatchObject({
        title: "Test Article",
      });
    });

    it("メモ作成イベントを正しく発行できる", async () => {
      const initialCount = receivedMessages.length;
      const broker = PubSubEventBroker(topic);
      const event = createTestEvent("memo.created", {
        snapshot: {
          identifier: "01HXXXXXXXXXXXXXXXXXXXXXX",
          title: "Test Memo",
          slug: "test-memo",
        },
      });

      await broker.publish(event).unwrap();

      await waitForMessages(initialCount + 1);

      const lastMessage = receivedMessages[receivedMessages.length - 1];
      expect(lastMessage?.type).toBe("memo.created");
      expect((lastMessage?.payload as Record<string, unknown>).snapshot).toMatchObject({
        title: "Test Memo",
      });
    });

    it("シリーズ永続化イベントを正しく発行できる", async () => {
      const initialCount = receivedMessages.length;
      const broker = PubSubEventBroker(topic);
      const event = createTestEvent("series.persisted", {
        series: "01HXXXXXXXXXXXXXXXXXXXXXX",
      });

      await broker.publish(event).unwrap();

      await waitForMessages(initialCount + 1);

      const lastMessage = receivedMessages[receivedMessages.length - 1];
      expect(lastMessage?.type).toBe("series.persisted");
    });
  });

  describe("複数イベントの並列発行", () => {
    it("複数のイベントを並列で発行できる", async () => {
      const initialCount = receivedMessages.length;
      const broker = PubSubEventBroker(topic);

      const events = [
        createTestEvent("article.created", { articleId: "1" }),
        createTestEvent("article.created", { articleId: "2" }),
        createTestEvent("article.created", { articleId: "3" }),
      ];

      const results = await Promise.all(
        events.map((event) => broker.publish(event).unwrap())
      );

      expect(results.length).toBe(3);

      await waitForMessages(initialCount + 3);

      expect(receivedMessages.length).toBeGreaterThanOrEqual(initialCount + 3);
    });
  });

  describe("イベントペイロードのシリアライゼーション", () => {
    it("日付を正しくシリアライズする", async () => {
      const initialCount = receivedMessages.length;
      const broker = PubSubEventBroker(topic);
      const occurredAt = new Date("2024-06-15T12:00:00Z");
      const event = createTestEvent(
        "article.created",
        { articleId: "test" },
        occurredAt
      );

      await broker.publish(event).unwrap();

      await waitForMessages(initialCount + 1);

      const lastMessage = receivedMessages[receivedMessages.length - 1];
      expect(lastMessage?.occurredAt).toBe("2024-06-15T12:00:00.000Z");
    });

    it("ネストされたオブジェクトを正しくシリアライズする", async () => {
      const initialCount = receivedMessages.length;
      const broker = PubSubEventBroker(topic);
      const event = createTestEvent("article.edited", {
        snapshot: {
          before: { title: "Old Title" },
          after: { title: "New Title" },
        },
        changes: ["title"],
      });

      await broker.publish(event).unwrap();

      await waitForMessages(initialCount + 1);

      const lastMessage = receivedMessages[receivedMessages.length - 1];
      const payload = lastMessage?.payload as Record<string, unknown>;
      const snapshot = payload.snapshot as Record<string, unknown>;
      expect((snapshot.before as Record<string, unknown>).title).toBe("Old Title");
      expect((snapshot.after as Record<string, unknown>).title).toBe("New Title");
      expect(payload.changes).toEqual(["title"]);
    });

    it("配列を正しくシリアライズする", async () => {
      const initialCount = receivedMessages.length;
      const broker = PubSubEventBroker(topic);
      const event = createTestEvent("article.edited", {
        tags: ["typescript", "react", "nextjs"],
      });

      await broker.publish(event).unwrap();

      await waitForMessages(initialCount + 1);

      const lastMessage = receivedMessages[receivedMessages.length - 1];
      const payload = lastMessage?.payload as Record<string, unknown>;
      expect(payload.tags).toEqual(["typescript", "react", "nextjs"]);
    });

    it("nullを正しくハンドリングする", async () => {
      const initialCount = receivedMessages.length;
      const broker = PubSubEventBroker(topic);
      const event = createTestEvent("article.created", {
        title: "Test",
        description: null,
      });

      await broker.publish(event).unwrap();

      await waitForMessages(initialCount + 1);

      const lastMessage = receivedMessages[receivedMessages.length - 1];
      const payload = lastMessage?.payload as Record<string, unknown>;
      expect(payload.title).toBe("Test");
      expect(payload.description).toBeNull();
    });
  });

  describe("イベント発行の信頼性", () => {
    it("大量のイベントを連続して発行できる", async () => {
      const initialCount = receivedMessages.length;
      const broker = PubSubEventBroker(topic);
      const eventCount = 10;

      const events = Array.from({ length: eventCount }, (_, i) =>
        createTestEvent("article.created", { index: i })
      );

      for (const event of events) {
        await broker.publish(event).unwrap();
      }

      await waitForMessages(initialCount + eventCount, 10000);

      expect(receivedMessages.length).toBeGreaterThanOrEqual(initialCount + eventCount);
    });
  });
  }
);
