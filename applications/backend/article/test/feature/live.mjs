import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import path from "node:path";
import { createTestHarness } from "wrangler";

const root = process.cwd();
const articleIdentifier = "01ARZ3NDEKTSV4RRFFQ69G5FC0";
const imageIdentifier = "01ARZ3NDEKTSV4RRFFQ69G5FA1";
const slug = "haskell-syntax-live-ai";
const timeoutMs = 45_000;

async function main() {
  for (const worker of ["article-do-worker", "article-excerpt-worker", "article-completion-worker"]) {
    const artifact = await readFile(
      path.join(root, `runtime/${worker}/generated/application-jsffi.mjs`),
      "utf8",
    );
    assert.match(artifact, /generatedArtifactKind = "generated"/);
  }

  const harness = createTestHarness({
    root,
    workers: [
      "driver.jsonc",
      "do.jsonc",
      "media.jsonc",
      "projection-sink.jsonc",
      "excerpt-live.jsonc",
      "completion.jsonc",
    ].map((name) => ({ configPath: path.join(root, "test/feature/wrangler", name) })),
  });
  try {
    const { url } = await harness.listen();
    const storage = await harness
      .getWorker("hut-article-api-worker-feature")
      .getDurableObjectStorage("ARTICLE_DO", { name: "articles" });
    const doRoute = (path) =>
      `${url.origin}/article-do?path=${encodeURIComponent(path)}`;
    const apiRoute = (path) =>
      `${url.origin}/article-api?path=${encodeURIComponent(path)}`;

    await fetch(doRoute("/internal/excerpt-generation/claim"));
    await storage.exec(
      "INSERT INTO article_aggregates " +
        "(identifier, slug, phase, updated_order, published_order, payload, revision) " +
        "VALUES (?, ?, 'unvalidated', '20260101000000000000000000', NULL, ?, 1)",
      articleIdentifier,
      slug,
      JSON.stringify({
        phase: "unvalidated",
        identifier: articleIdentifier,
        title: "Haskell syntax",
        body: [
          "# Haskell syntax",
          "",
          "A Haskell function applies to its argument using a space.",
          "For example, square 3 applies square to 3.",
          "",
          `![Syntax diagram](https://assets.hut.dev.lihs-dev.com/images/${imageIdentifier})`,
        ].join("\n"),
        slug,
        excerpt: null,
        tags: [],
        images: [imageIdentifier],
        createdAt: "2026-01-01T00:00:00Z",
        updatedAt: "2026-01-01T00:00:00Z",
        publishedAt: null,
      }),
    );

    const response = await fetch(
      apiRoute(`/admin/articles/${articleIdentifier}/proofreading`),
      { method: "POST", headers: { "X-Hut-Actor": "editor" } },
    );
    assert.equal(response.status, 200, await response.clone().text());

    const deadline = Date.now() + timeoutMs;
    while (Date.now() < deadline) {
      const rows = await storage.exec(
        "SELECT payload, revision FROM article_aggregates WHERE identifier = ?",
        articleIdentifier,
      );
      const stored = JSON.parse(rows[0].payload);
      if (stored.phase === "ready") {
        assert.ok(stored.excerpt?.length > 0);
        assert.ok(stored.excerpt.length <= 200);
        assert.deepEqual(stored.images, [imageIdentifier]);
        assert.equal(rows[0].revision, 3);
        console.log(`real Workers AI generated an excerpt: ${stored.excerpt}`);
        return;
      }
      await new Promise((resolve) => setTimeout(resolve, 1000));
    }

    const article = await storage.exec(
      "SELECT payload, revision FROM article_aggregates WHERE identifier = ?",
      articleIdentifier,
    );
    const jobs = await storage.exec(
      "SELECT status, revision FROM article_generation_jobs WHERE article_identifier = ?",
      articleIdentifier,
    );
    const outbox = await storage.exec(
      "SELECT event_kind, status, attempts FROM article_outbox WHERE article_identifier = ?",
      articleIdentifier,
    );
    harness.debug();
    throw new Error(`AI flow timed out: ${JSON.stringify({ article, jobs, outbox })}`);
  } finally {
    await harness.close();
  }
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
