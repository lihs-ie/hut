/**
 * @vitest-environment node
 */
import { describe, it, expect } from "vitest";

describe("revalidation constants", () => {
  it("ARTICLES タグが 'articles' である", async () => {
    const { REVALIDATION_TAGS } = await import("@shared/config/revalidation");
    expect(REVALIDATION_TAGS.ARTICLES).toBe("articles");
  });

  it("MEMOS タグが 'memos' である", async () => {
    const { REVALIDATION_TAGS } = await import("@shared/config/revalidation");
    expect(REVALIDATION_TAGS.MEMOS).toBe("memos");
  });

  it("SERIES タグが 'series' である", async () => {
    const { REVALIDATION_TAGS } = await import("@shared/config/revalidation");
    expect(REVALIDATION_TAGS.SERIES).toBe("series");
  });

  it("CHAPTERS タグが 'chapters' である", async () => {
    const { REVALIDATION_TAGS } = await import("@shared/config/revalidation");
    expect(REVALIDATION_TAGS.CHAPTERS).toBe("chapters");
  });

  it("TAGS タグが 'tags' である", async () => {
    const { REVALIDATION_TAGS } = await import("@shared/config/revalidation");
    expect(REVALIDATION_TAGS.TAGS).toBe("tags");
  });

  it("PRIVACY_POLICY タグが 'privacy-policy' である", async () => {
    const { REVALIDATION_TAGS } = await import("@shared/config/revalidation");
    expect(REVALIDATION_TAGS.PRIVACY_POLICY).toBe("privacy-policy");
  });

  it("memoEntriesTag が slug から動的タグ文字列を生成する", async () => {
    const { memoEntriesTag } = await import("@shared/config/revalidation");
    expect(memoEntriesTag("my-memo")).toBe("memo-entries-my-memo");
  });

  it("memoEntriesTag('foo') が 'memo-entries-foo' を返す", async () => {
    const { memoEntriesTag } = await import("@shared/config/revalidation");
    expect(memoEntriesTag("foo")).toBe("memo-entries-foo");
  });
});
