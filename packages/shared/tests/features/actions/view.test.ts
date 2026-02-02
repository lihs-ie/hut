/**
 * View Action Feature Test
 *
 * Firebase Emulatorを使用してビューカウント永続化の統合テストを行います。
 */

import { describe, it, expect, beforeEach, afterAll, vi } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  incrementViewCountCore,
  getJstDateKey,
  type PageViewCounter,
  type PageViewDedup,
} from "@shared/actions/view";
import { ContentType } from "@shared/domains/search-token/reference";
import {
  createFeatureTestContext,
  cleanupFeatureTest,
  type FeatureTestContext,
} from "../setup";
import { SearchReferenceIdentifierMold } from "../../support/molds/domains/search-token";
import { randomUUID } from "crypto";

describe("Feature: View Action (実DB接続)", () => {
  let context: FeatureTestContext;

  beforeEach(async () => {
    vi.spyOn(console, "log").mockImplementation(() => {});

    context = await createFeatureTestContext();
  }, 30000);

  afterAll(async () => {
    await cleanupFeatureTest();
  }, 30000);

  describe("ビューカウントの永続化", () => {
    it("新規ビューカウントが正しく永続化される", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(1, {
        type: ContentType.ARTICLE,
      });
      const sessionKey = randomUUID();
      const now = new Date("2024-01-15T10:00:00Z");
      const dateKey = getJstDateKey(now);

      await incrementViewCountCore({
        firestore: context.firestore,
        operations: context.operations,
        identifier,
        sessionKey,
        now,
      });

      // カウンターが永続化されていることを確認
      const counterDoc = context.operations.doc<PageViewCounter>(
        context.firestore,
        "page-view-counters",
        identifier.type,
        identifier.content,
        dateKey,
      );
      const counterSnapshot = await context.operations.getDoc(counterDoc);

      expect(counterSnapshot.exists()).toBe(true);
      const counterData = counterSnapshot.data();
      expect(counterData?.count).toBe(1);
      expect(counterData?.updatedAt).toBe(now.toISOString());

      // dedupドキュメントが永続化されていることを確認
      const dedupDocId = `${dateKey}:${sessionKey}`;
      const dedupDoc = context.operations.doc<PageViewDedup>(
        context.firestore,
        "page-view-dedup",
        identifier.type,
        identifier.content,
        dedupDocId,
      );
      const dedupSnapshot = await context.operations.getDoc(dedupDoc);

      expect(dedupSnapshot.exists()).toBe(true);
      const dedupData = dedupSnapshot.data();
      expect(dedupData?.createdAt).toBe(now.toISOString());
    });

    it("同一セッションでの重複カウントが防止される", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(2, {
        type: ContentType.MEMO,
      });
      const sessionKey = randomUUID();
      const now = new Date("2024-01-15T10:00:00Z");
      const dateKey = getJstDateKey(now);

      // 1回目のビューカウント
      await incrementViewCountCore({
        firestore: context.firestore,
        operations: context.operations,
        identifier,
        sessionKey,
        now,
      });

      // 2回目のビューカウント（同一セッション）
      await incrementViewCountCore({
        firestore: context.firestore,
        operations: context.operations,
        identifier,
        sessionKey,
        now: new Date("2024-01-15T10:30:00Z"),
      });

      // カウンターが1のままであることを確認
      const counterDoc = context.operations.doc<PageViewCounter>(
        context.firestore,
        "page-view-counters",
        identifier.type,
        identifier.content,
        dateKey,
      );
      const counterSnapshot = await context.operations.getDoc(counterDoc);

      expect(counterSnapshot.exists()).toBe(true);
      const counterData = counterSnapshot.data();
      expect(counterData?.count).toBe(1);
    });

    it("異なるセッションからのビューカウントは加算される", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(3, {
        type: ContentType.SERIES,
      });
      const sessionKey1 = randomUUID();
      const sessionKey2 = randomUUID();
      const now = new Date("2024-01-15T10:00:00Z");
      const dateKey = getJstDateKey(now);

      // 1回目のビューカウント（セッション1）
      await incrementViewCountCore({
        firestore: context.firestore,
        operations: context.operations,
        identifier,
        sessionKey: sessionKey1,
        now,
      });

      // 2回目のビューカウント（セッション2）
      await incrementViewCountCore({
        firestore: context.firestore,
        operations: context.operations,
        identifier,
        sessionKey: sessionKey2,
        now: new Date("2024-01-15T10:30:00Z"),
      });

      // カウンターが2であることを確認
      const counterDoc = context.operations.doc<PageViewCounter>(
        context.firestore,
        "page-view-counters",
        identifier.type,
        identifier.content,
        dateKey,
      );
      const counterSnapshot = await context.operations.getDoc(counterDoc);

      expect(counterSnapshot.exists()).toBe(true);
      const counterData = counterSnapshot.data();
      expect(counterData?.count).toBe(2);
    });

    it("日付が変わると新しいカウンターが作成される", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(4, {
        type: ContentType.ARTICLE,
      });
      const sessionKey = randomUUID();
      const day1 = new Date("2024-01-15T10:00:00Z");
      const day2 = new Date("2024-01-16T10:00:00Z");
      const dateKey1 = getJstDateKey(day1);
      const dateKey2 = getJstDateKey(day2);

      // 1日目のビューカウント
      await incrementViewCountCore({
        firestore: context.firestore,
        operations: context.operations,
        identifier,
        sessionKey,
        now: day1,
      });

      // 2日目のビューカウント（同一セッションでも日付が異なれば別カウント）
      await incrementViewCountCore({
        firestore: context.firestore,
        operations: context.operations,
        identifier,
        sessionKey,
        now: day2,
      });

      // 1日目のカウンターを確認
      const counterDoc1 = context.operations.doc<PageViewCounter>(
        context.firestore,
        "page-view-counters",
        identifier.type,
        identifier.content,
        dateKey1,
      );
      const counterSnapshot1 = await context.operations.getDoc(counterDoc1);
      expect(counterSnapshot1.exists()).toBe(true);
      expect(counterSnapshot1.data()?.count).toBe(1);

      // 2日目のカウンターを確認
      const counterDoc2 = context.operations.doc<PageViewCounter>(
        context.firestore,
        "page-view-counters",
        identifier.type,
        identifier.content,
        dateKey2,
      );
      const counterSnapshot2 = await context.operations.getDoc(counterDoc2);
      expect(counterSnapshot2.exists()).toBe(true);
      expect(counterSnapshot2.data()?.count).toBe(1);
    });
  });

  describe("並列処理の最適化", () => {
    it("複数の異なるコンテンツへのビューカウントが並列で実行できる", async () => {
      const identifiers = Forger(SearchReferenceIdentifierMold).forgeMultiWithSeed(3, 10);
      const sessionKey = randomUUID();
      const now = new Date("2024-01-15T10:00:00Z");
      const dateKey = getJstDateKey(now);

      // 並列でビューカウント
      await Promise.all(
        identifiers.map((identifier) =>
          incrementViewCountCore({
            firestore: context.firestore,
            operations: context.operations,
            identifier,
            sessionKey,
            now,
          }),
        ),
      );

      // 各コンテンツのカウンターが永続化されていることを確認
      for (const identifier of identifiers) {
        const counterDoc = context.operations.doc<PageViewCounter>(
          context.firestore,
          "page-view-counters",
          identifier.type,
          identifier.content,
          dateKey,
        );
        const counterSnapshot = await context.operations.getDoc(counterDoc);

        expect(counterSnapshot.exists()).toBe(true);
        expect(counterSnapshot.data()?.count).toBe(1);
      }
    });

    it("複数の異なるセッションからの同一コンテンツへのビューカウントが並列で実行できる", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(20, {
        type: ContentType.ARTICLE,
      });
      const sessionKeys = Array.from({ length: 5 }, () => randomUUID());
      const now = new Date("2024-01-15T10:00:00Z");
      const dateKey = getJstDateKey(now);

      // 並列でビューカウント
      await Promise.all(
        sessionKeys.map((sessionKey) =>
          incrementViewCountCore({
            firestore: context.firestore,
            operations: context.operations,
            identifier,
            sessionKey,
            now,
          }),
        ),
      );

      // カウンターが5であることを確認
      const counterDoc = context.operations.doc<PageViewCounter>(
        context.firestore,
        "page-view-counters",
        identifier.type,
        identifier.content,
        dateKey,
      );
      const counterSnapshot = await context.operations.getDoc(counterDoc);

      expect(counterSnapshot.exists()).toBe(true);
      expect(counterSnapshot.data()?.count).toBe(5);
    });
  });

  describe("JST日付キーの生成", () => {
    it("UTC時刻からJST日付キーが正しく生成される", () => {
      // UTC 2024-01-15 00:00:00 -> JST 2024-01-15 09:00:00
      const utcMidnight = new Date("2024-01-15T00:00:00Z");
      expect(getJstDateKey(utcMidnight)).toBe("2024-01-15");

      // UTC 2024-01-14 15:00:00 -> JST 2024-01-15 00:00:00
      const jstMidnight = new Date("2024-01-14T15:00:00Z");
      expect(getJstDateKey(jstMidnight)).toBe("2024-01-15");

      // UTC 2024-01-14 14:59:59 -> JST 2024-01-14 23:59:59
      const beforeJstMidnight = new Date("2024-01-14T14:59:59Z");
      expect(getJstDateKey(beforeJstMidnight)).toBe("2024-01-14");
    });
  });

  describe("データ永続化の検証", () => {
    it("ビューカウントのデータが正確に永続化される", async () => {
      const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(30, {
        type: ContentType.ARTICLE,
      });
      const sessionKey = randomUUID();
      const now = new Date("2024-01-15T12:34:56.789Z");
      const dateKey = getJstDateKey(now);

      await incrementViewCountCore({
        firestore: context.firestore,
        operations: context.operations,
        identifier,
        sessionKey,
        now,
      });

      // カウンターデータの検証
      const counterDoc = context.operations.doc<PageViewCounter>(
        context.firestore,
        "page-view-counters",
        identifier.type,
        identifier.content,
        dateKey,
      );
      const counterSnapshot = await context.operations.getDoc(counterDoc);
      const counterData = counterSnapshot.data();

      expect(counterData?.count).toBe(1);
      expect(counterData?.updatedAt).toBe(now.toISOString());

      // dedupデータの検証
      const dedupDocId = `${dateKey}:${sessionKey}`;
      const dedupDoc = context.operations.doc<PageViewDedup>(
        context.firestore,
        "page-view-dedup",
        identifier.type,
        identifier.content,
        dedupDocId,
      );
      const dedupSnapshot = await context.operations.getDoc(dedupDoc);
      const dedupData = dedupSnapshot.data();

      expect(dedupData?.createdAt).toBe(now.toISOString());
    });
  });

  describe("コンテンツタイプ別のビューカウント", () => {
    it("Article、Memo、Seriesの各タイプでビューカウントが正しく動作する", async () => {
      const contentTypes = [
        ContentType.ARTICLE,
        ContentType.MEMO,
        ContentType.SERIES,
      ];
      const sessionKey = randomUUID();
      const now = new Date("2024-01-15T10:00:00Z");
      const dateKey = getJstDateKey(now);

      for (const type of contentTypes) {
        const identifier = Forger(SearchReferenceIdentifierMold).forgeWithSeed(
          40 + contentTypes.indexOf(type),
          { type },
        );

        await incrementViewCountCore({
          firestore: context.firestore,
          operations: context.operations,
          identifier,
          sessionKey,
          now,
        });

        const counterDoc = context.operations.doc<PageViewCounter>(
          context.firestore,
          "page-view-counters",
          identifier.type,
          identifier.content,
          dateKey,
        );
        const counterSnapshot = await context.operations.getDoc(counterDoc);

        expect(counterSnapshot.exists()).toBe(true);
        expect(counterSnapshot.data()?.count).toBe(1);
      }
    });
  });
});
