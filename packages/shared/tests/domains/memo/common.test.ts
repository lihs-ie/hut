import { describe, it, expect } from "vitest";
import { Forger } from "@lihs-ie/forger-ts";
import {
  memoIdentifierSchema,
  validateMemoIdentifier,
  titleSchema,
  entrySchema,
  memoSchema,
  validateMemo,
  validateEntry,
  addEntry,
  toSnapshot,
  criteriaSchema,
  validateCriteria,
} from "@shared/domains/memo";
import {
  MemoMold,
  MemoIdentifierMold,
  MemoEntryMold,
} from "../../support/molds/domains/memo";
import { TimelineMold } from "../../support/molds/domains/common/date";
import { TagIdentifierMold } from "../../support/molds/domains/attributes/tag";
import {
  describeIdentifierSchema,
  describeStringLengthSchema,
} from "../../support/helpers";

describe("domains/memo/common", () => {
  describe("memoIdentifierSchema", () => {
    describeIdentifierSchema(
      "MemoIdentifier",
      memoIdentifierSchema,
      () => Forger(MemoIdentifierMold).forge(),
      (count) => Forger(MemoIdentifierMold).forgeMulti(count)
    );
  });

  describe("validateMemoIdentifier", () => {
    it("有効なULIDでokを返す", () => {
      const result = validateMemoIdentifier(Forger(MemoIdentifierMold).forge());
      expect(result.isOk).toBe(true);
    });

    it("無効な文字列でerrを返す", () => {
      const result = validateMemoIdentifier("invalid");
      expect(result.isErr).toBe(true);
      if (result.isErr) {
        expect(result.unwrapError().field).toBe("MemoIdentifier");
      }
    });
  });

  describe("titleSchema", () => {
    describeStringLengthSchema("タイトル", titleSchema, 1, 100, {
      minLengthMessage: "Title must be at least 1 character long",
      maxLengthMessage: "Title must be at most 100 characters long",
    });

    it("日本語を含むタイトルは有効", () => {
      const result = titleSchema.safeParse("テストメモタイトル");
      expect(result.success).toBe(true);
    });
  });

  describe("entrySchema", () => {
    describe("有効なエントリーの検証", () => {
      it("1文字のテキストは有効", () => {
        const result = entrySchema.safeParse({ text: "A", createdAt: new Date() });
        expect(result.success).toBe(true);
      });

      it("1000文字のテキストは有効", () => {
        const result = entrySchema.safeParse({ text: "a".repeat(1000), createdAt: new Date() });
        expect(result.success).toBe(true);
      });

      it("Forgerで生成したエントリーは有効", () => {
        const result = entrySchema.safeParse(Forger(MemoEntryMold).forge());
        expect(result.success).toBe(true);
      });
    });

    describe("無効なエントリーの検証", () => {
      it("空文字列のテキストは無効", () => {
        const result = entrySchema.safeParse({ text: "", createdAt: new Date() });
        expect(result.success).toBe(false);
        if (!result.success) {
          expect(result.error.issues[0].message).toBe("Content text must be at least 1 character long");
        }
      });

      it("1001文字以上のテキストは無効", () => {
        const result = entrySchema.safeParse({ text: "a".repeat(1001), createdAt: new Date() });
        expect(result.success).toBe(false);
      });

      it("createdAtが欠けている場合は無効", () => {
        const result = entrySchema.safeParse({ text: "Test" });
        expect(result.success).toBe(false);
      });

      it("createdAtがDateでない場合は無効", () => {
        const result = entrySchema.safeParse({ text: "Test", createdAt: "2024-01-01" });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateEntry", () => {
    it("有効なエントリーでokを返す", () => {
      const result = validateEntry({ text: "テストエントリー", createdAt: new Date() });
      expect(result.isOk).toBe(true);
    });

    it("無効なエントリーでerrを返す", () => {
      const result = validateEntry({ text: "", createdAt: new Date() });
      expect(result.isErr).toBe(true);
    });
  });

  describe("memoSchema", () => {
    describe("有効なMemoの検証", () => {
      it("全てのフィールドが有効な場合は検証を通過する", () => {
        const result = memoSchema.safeParse(Forger(MemoMold).forge());
        expect(result.success).toBe(true);
      });

      it("エントリーが空配列でも有効", () => {
        const result = memoSchema.safeParse(Forger(MemoMold).forge({ entries: [] }));
        expect(result.success).toBe(true);
      });

      it("タグが空配列でも有効", () => {
        const result = memoSchema.safeParse(Forger(MemoMold).forge({ tags: [] }));
        expect(result.success).toBe(true);
      });
    });

    describe("無効なMemoの検証", () => {
      const createMemoWithOverrides = (overrides: Record<string, unknown>) => ({
        identifier: Forger(MemoIdentifierMold).forge(),
        title: "Test",
        slug: "test-slug",
        entries: [],
        tags: [],
        status: "draft",
        timeline: { createdAt: new Date(), updatedAt: new Date() },
        ...overrides,
      });

      it("identifierが無効な場合は無効", () => {
        const result = memoSchema.safeParse(createMemoWithOverrides({ identifier: "invalid" }));
        expect(result.success).toBe(false);
      });

      it("titleが空の場合は無効", () => {
        const result = memoSchema.safeParse(createMemoWithOverrides({ title: "" }));
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateMemo", () => {
    it("有効なUnvalidatedMemoでokを返す", () => {
      const result = validateMemo({
        identifier: Forger(MemoIdentifierMold).forge(),
        title: "テストメモ",
        slug: "test-memo",
        entries: [],
        tags: [],
        status: "draft",
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isOk).toBe(true);
    });

    it("無効なUnvalidatedMemoでerrを返す", () => {
      const result = validateMemo({
        identifier: "invalid",
        title: "",
        slug: "",
        entries: [],
        tags: [],
        status: "invalid",
        timeline: Forger(TimelineMold).forge(),
      });
      expect(result.isErr).toBe(true);
    });
  });

  describe("addEntry", () => {
    it("メモに新しいエントリーを追加する", () => {
      const memo = Forger(MemoMold).forge({ entries: [] });
      const newEntry = Forger(MemoEntryMold).forge();
      const updatedMemo = addEntry(memo, newEntry);

      expect(updatedMemo.entries).toHaveLength(1);
      expect(updatedMemo.entries[0]).toEqual(newEntry);
    });

    it("既存のエントリーに追加される", () => {
      const existingEntries = Forger(MemoEntryMold).forgeMulti(2);
      const memo = Forger(MemoMold).forge({ entries: existingEntries });
      const newEntry = Forger(MemoEntryMold).forge();
      const updatedMemo = addEntry(memo, newEntry);

      expect(updatedMemo.entries).toHaveLength(3);
      expect(updatedMemo.entries[2]).toEqual(newEntry);
    });

    it("timelineのupdatedAtがエントリーのcreatedAtに更新される", () => {
      const memo = Forger(MemoMold).forge();
      const newEntryDate = new Date("2025-12-31T00:00:00Z");
      const newEntry = Forger(MemoEntryMold).forge({ createdAt: newEntryDate });
      const updatedMemo = addEntry(memo, newEntry);

      expect(updatedMemo.timeline.updatedAt).toEqual(newEntryDate);
    });

    it("元のメモは変更されない（イミュータブル）", () => {
      const memo = Forger(MemoMold).forge({ entries: [] });
      const newEntry = Forger(MemoEntryMold).forge();
      addEntry(memo, newEntry);

      expect(memo.entries).toHaveLength(0);
    });
  });

  describe("toSnapshot", () => {
    it("MemoからMemoSnapshotを作成する", () => {
      const memo = Forger(MemoMold).forge();
      const snapshot = toSnapshot(memo);

      expect(snapshot.identifier).toBe(memo.identifier);
      expect(snapshot.title).toBe(memo.title);
      expect(snapshot.status).toBe(memo.status);
    });
  });

  describe("criteriaSchema", () => {
    describe("有効なCriteriaの検証", () => {
      it("全てnullで有効", () => {
        const result = criteriaSchema.safeParse({ tags: null, freeWord: null, status: null });
        expect(result.success).toBe(true);
      });

      it("全てのフィールドを指定しても有効", () => {
        const result = criteriaSchema.safeParse({
          tags: Forger(TagIdentifierMold).forgeMulti(2),
          freeWord: "検索ワード",
          status: "published",
        });
        expect(result.success).toBe(true);
      });
    });

    describe("無効なCriteriaの検証", () => {
      it("freeWordが空文字列の場合は無効", () => {
        const result = criteriaSchema.safeParse({ tags: null, freeWord: "", status: null });
        expect(result.success).toBe(false);
      });

      it("freeWordが101文字以上の場合は無効", () => {
        const result = criteriaSchema.safeParse({ tags: null, freeWord: "a".repeat(101), status: null });
        expect(result.success).toBe(false);
      });

      it("無効なstatusの場合は無効", () => {
        const result = criteriaSchema.safeParse({ tags: null, freeWord: null, status: "invalid-status" });
        expect(result.success).toBe(false);
      });
    });
  });

  describe("validateCriteria", () => {
    it("有効なCriteriaでokを返す", () => {
      const result = validateCriteria({ tags: null, freeWord: "テスト", status: "published" });
      expect(result.isOk).toBe(true);
    });

    it("無効なCriteriaでerrを返す", () => {
      const result = validateCriteria({ tags: null, freeWord: "", status: null });
      expect(result.isErr).toBe(true);
    });
  });
});
