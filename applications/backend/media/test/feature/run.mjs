import { spawn } from "node:child_process";
import { readFile } from "node:fs/promises";
import path from "node:path";
import process from "node:process";
import { createTestHarness } from "wrangler";

const root = process.cwd();
const feature = path.join(root, "test/feature");
const runtimeNames = [
  "media-api-worker",
  "media-inspection-worker",
  "media-reference-projection-worker",
  "media-retention-worker",
];

function commandEnvironment() {
  const environment = { ...process.env };
  delete environment.CLOUDFLARE_API_TOKEN;
  delete environment.CLOUDFLARE_ACCOUNT_ID;
  environment.WRANGLER_SEND_METRICS = "false";
  return environment;
}

async function rejectStubArtifacts() {
  for (const runtimeName of runtimeNames) {
    const loader = path.join(
      root,
      "runtime",
      runtimeName,
      "generated/application-jsffi.mjs",
    );
    const source = await readFile(loader, "utf8");
    if (/generatedArtifactKind\s*=\s*["']stub["']/.test(source)) {
      throw new Error(`${runtimeName} has not been built for wasm32-wasi`);
    }
  }
}

function run(command, arguments_, options = {}) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, arguments_, {
      cwd: root,
      env: commandEnvironment(),
      stdio: "inherit",
      ...options,
    });
    child.once("error", reject);
    child.once("exit", (code, signal) => {
      if (code === 0) resolve();
      else reject(new Error(`${command} exited with ${code ?? signal}`));
    });
  });
}

async function runSpecs(baseURL, ...names) {
  await run(
    process.execPath,
    ["--test", ...names.map((name) => path.join(feature, "spec", name))],
    { env: { ...commandEnvironment(), FEATURE_BASE_URL: baseURL } },
  );
}

async function main() {
  await rejectStubArtifacts();
  const server = createTestHarness({
    root,
    workers: [
      { configPath: path.join(feature, "wrangler/driver.jsonc") },
      { configPath: path.join(feature, "wrangler/api.jsonc") },
      { configPath: path.join(feature, "wrangler/inspection.jsonc") },
      { configPath: path.join(feature, "wrangler/projection.jsonc") },
    ],
  });
  try {
    const { url } = await server.listen();
    const baseURL = url.origin;
    const driver = server.getWorker("hut-media-feature-driver");
    await driver.applyD1Migrations("MEDIA_DATABASE");
    await runSpecs(
      baseURL,
      "resources.test.mjs",
      "http.test.mjs",
      "queues.test.mjs",
    );
  } finally {
    await server.close();
  }
  await runSpecs("", "scheduled.test.mjs");
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
