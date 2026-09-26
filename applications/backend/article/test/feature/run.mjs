import { spawn } from "node:child_process";
import { readFile } from "node:fs/promises";
import path from "node:path";
import process from "node:process";
import { createTestHarness } from "wrangler";

const root = process.cwd();

async function main() {
  const loader = path.join(
    root,
    "runtime/article-do-worker/generated/application-jsffi.mjs",
  );
  const artifact = await readFile(loader, "utf8");
  if (!artifact.includes('generatedArtifactKind = "generated"')) {
    throw new Error("Article DO requires a generated WASM artifact");
  }

  const harness = createTestHarness({
    root,
    workers: [
      { configPath: path.join(root, "test/feature/wrangler/driver.jsonc") },
      { configPath: path.join(root, "test/feature/wrangler/do.jsonc") },
      { configPath: path.join(root, "test/feature/wrangler/media.jsonc") },
    ],
  });
  try {
    const { url } = await harness.listen();
    const worker = harness.getWorker("hut-article-api-worker-feature");
    const storage = await worker.getDurableObjectStorage("ARTICLE_DO", {
      name: "articles",
    });
    await fetch(`${url.origin}/article-do?path=${encodeURIComponent("/internal/excerpt-generation/claim")}`);
    const drafts = [
      ["01ARZ3NDEKTSV4RRFFQ69G5FB0", "01ARZ3NDEKTSV4RRFFQ69G5FA1"],
      ["01ARZ3NDEKTSV4RRFFQ69G5FB1", "01ARZ3NDEKTSV4RRFFQ69G5FA2"],
      ["01ARZ3NDEKTSV4RRFFQ69G5FB2", null],
    ];
    for (const [identifier, image] of drafts) {
      const payload = JSON.stringify({
        phase: "unvalidated",
        identifier,
        title: "Haskell syntax",
        body: "# Haskell syntax\n\nA practical introduction to expressions and types.",
        slug: `haskell-syntax-${identifier.toLowerCase()}`,
        excerpt: null,
        tags: [],
        images: image ? [image] : [],
        createdAt: "2026-01-01T00:00:00Z",
        updatedAt: "2026-01-01T00:00:00Z",
        publishedAt: null,
      });
      await storage.exec(
        "INSERT INTO article_aggregates (identifier, slug, payload, revision) VALUES (?, ?, ?, 1)",
        identifier,
        `haskell-syntax-${identifier.toLowerCase()}`,
        payload,
      );
    }
    await new Promise((resolve, reject) => {
      const child = spawn(process.execPath, [
        "--test",
        path.join(root, "test/feature/spec/do.test.mjs"),
      ], {
        cwd: root,
        env: {
          ...process.env,
          FEATURE_BASE_URL: url.origin,
          WRANGLER_SEND_METRICS: "false",
        },
        stdio: "inherit",
      });
      child.once("error", reject);
      child.once("exit", (code) => {
        if (code === 0) resolve();
        else reject(new Error(`feature tests exited with ${code}`));
      });
    });
  } finally {
    await harness.close();
  }
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
