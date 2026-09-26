import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { createHash } from "node:crypto";
import { readFile, rm, writeFile } from "node:fs/promises";
import path from "node:path";
import process from "node:process";

const root = process.cwd();
const remoteDirectory = path.join(root, "test/feature/remote");
const driverPort = 8788;
const retentionPort = 8789;
const driverURL = `http://127.0.0.1:${driverPort}`;
const publicBaseURL = "https://assets.hut.dev.lihs-dev.com";
const requiredEnvironment = [
  "CLOUDFLARE_ACCOUNT_ID",
  "CLOUDFLARE_API_TOKEN",
  "CLOUDFLARE_CACHE_PURGE_TOKEN",
  "CLOUDFLARE_ZONE_IDENTIFIER",
  "MEDIA_D1_DATABASE_ID",
  "MEDIA_REFERENCE_QUEUE_ID",
];

for (const name of requiredEnvironment) {
  assert.ok(process.env[name], `${name} is required`);
}

const fixtures = [
  ["png", "image/png", "image/png"],
  ["jpg", "image/jpeg", "image/webp"],
  ["webp", "image/webp", "image/webp"],
  ["heic", "image/heic", "image/webp"],
  ["gif", "image/gif", "image/gif"],
];

function timedFetch(input, init = {}, timeout = 15_000) {
  return fetch(input, { ...init, signal: AbortSignal.timeout(timeout) });
}

function configuration(main, bindings = {}) {
  return {
    $schema: "../../../node_modules/wrangler/config-schema.json",
    name: `hut-media-remote-smoke-${Date.now()}`,
    main,
    compatibility_date: "2026-09-13",
    compatibility_flags: ["nodejs_compat"],
    ...bindings,
  };
}

function remoteResources() {
  return {
    d1_databases: [
      {
        binding: "MEDIA_DATABASE",
        database_name: "hut-media-dev",
        database_id: process.env.MEDIA_D1_DATABASE_ID,
        remote: true,
      },
    ],
    r2_buckets: [
      {
        binding: "MEDIA_TMP_UPLOADS",
        bucket_name: "hut-media-tmp-uploads-dev",
        remote: true,
      },
      {
        binding: "MEDIA_ASSETS",
        bucket_name: "hut-media-assets-dev",
        remote: true,
      },
    ],
  };
}

async function writeConfigurations() {
  const driver = configuration("driver.mjs", {
    ...remoteResources(),
    services: [
      {
        binding: "MEDIA_API_WORKER",
        service: "hut-media-api-worker-dev",
        remote: true,
      },
    ],
  });
  const retention = configuration(
    "../../../runtime/media-retention-worker/src/index.ts",
    {
      ...remoteResources(),
      vars: {
        CLOUDFLARE_ZONE_IDENTIFIER:
          process.env.CLOUDFLARE_ZONE_IDENTIFIER,
        CLOUDFLARE_CACHE_PURGE_TOKEN:
          process.env.CLOUDFLARE_CACHE_PURGE_TOKEN,
        MEDIA_PUBLIC_BASE_URL: publicBaseURL,
      },
    },
  );
  const driverPath = path.join(remoteDirectory, ".driver.wrangler.json");
  const retentionPath = path.join(
    remoteDirectory,
    ".retention.wrangler.json",
  );
  await writeFile(driverPath, `${JSON.stringify(driver, null, 2)}\n`);
  await writeFile(retentionPath, `${JSON.stringify(retention, null, 2)}\n`);
  return { driverPath, retentionPath };
}

function startWrangler(configPath, port, extraArguments = []) {
  const child = spawn(
    "corepack",
    [
      "pnpm@12.4.2",
      "exec",
      "wrangler",
      "dev",
      "--config",
      configPath,
      "--ip",
      "127.0.0.1",
      "--port",
      String(port),
      ...extraArguments,
    ],
    {
      cwd: root,
      detached: true,
      env: { ...process.env, WRANGLER_SEND_METRICS: "false" },
      stdio: ["ignore", "pipe", "pipe"],
    },
  );
  child.stdout.on("data", (chunk) => process.stdout.write(chunk));
  child.stderr.on("data", (chunk) => process.stderr.write(chunk));
  return child;
}

function stopWrangler(child) {
  if (child.exitCode !== null || child.signalCode !== null) return;
  try {
    process.kill(-child.pid, "SIGTERM");
  } catch (error) {
    if (error.code !== "ESRCH") throw error;
  }
}

async function eventually(label, operation, predicate, timeout = 60_000) {
  const deadline = Date.now() + timeout;
  let last;
  while (Date.now() < deadline) {
    try {
      last = await operation();
      if (predicate(last)) return last;
    } catch (error) {
      last = error;
    }
    await new Promise((resolve) => setTimeout(resolve, 500));
  }
  const diagnostic =
    last instanceof Error
      ? last.stack ?? last.message
      : JSON.stringify(last);
  throw new Error(`${label} timed out: ${diagnostic}`);
}

async function waitForReady(url) {
  await eventually(
    `Worker ${url}`,
    async () => (await timedFetch(url)).status,
    (status) => status === 200,
  );
}

async function waitForListening(url) {
  await eventually(
    `Worker ${url}`,
    async () => (await timedFetch(url)).status,
    (status) => Number.isInteger(status),
  );
}

async function driverJSON(pathname, init) {
  const response = await timedFetch(`${driverURL}${pathname}`, init);
  const body = await response.json();
  assert.ok(response.ok, `${pathname}: ${response.status} ${JSON.stringify(body)}`);
  return body;
}

async function uploadFixture(extension, inputType, outputType, trackedUploads) {
  const bytes = await readFile(
    path.join(remoteDirectory, "fixtures", `sample.${extension}`),
  );
  const digest = createHash("sha256").update(bytes).digest();
  const sha256 = digest.toString("hex");
  const checksum = digest.toString("base64");
  const issued = await driverJSON("/api/images", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ contentType: inputType, byteSize: bytes.length, sha256 }),
  });
  const upload = {
    identifier: issued.imageIdentifier,
    publicURL: `${publicBaseURL}/images/${issued.imageIdentifier}`,
  };
  trackedUploads.push(upload);
  const uploaded = await timedFetch(issued.uploadDestination, {
    method: "PUT",
    headers: {
      "content-type": inputType,
      "x-amz-checksum-sha256": checksum,
    },
    body: bytes,
  }, 60_000);
  assert.ok(uploaded.ok, `${extension} PUT failed: ${uploaded.status}`);
  const { identifier, publicURL } = upload;
  const state = await eventually(
    `${extension} inspection`,
    () => driverJSON(`/state/${identifier}`),
    (value) => ["available", "rejected"].includes(value.image?.state),
    90_000,
  );
  assert.equal(
    state.image.state,
    "available",
    `${extension} inspection failed: ${JSON.stringify(state)}`,
  );
  assert.equal(state.temporaryExists, false);
  assert.equal(state.finalExists, true);
  assert.ok(state.attempt.uploadedAt);
  assert.ok(state.inspection.inspectedAt);
  assert.ok(state.image.availableAt);
  assert.ok(state.attempt.uploadedAt <= state.inspection.inspectedAt);
  assert.equal(state.inspection.inspectedAt, state.image.availableAt);

  const first = await timedFetch(publicURL);
  assert.equal(first.status, 200);
  assert.match(first.headers.get("content-type") ?? "", new RegExp(`^${outputType}`));
  const output = new Uint8Array(await first.arrayBuffer());
  if (extension === "gif") {
    const frameControls = output.reduce(
      (count, byte, index) =>
        count +
        (byte === 0x21 && output[index + 1] === 0xf9 && output[index + 2] === 0x04
          ? 1
          : 0),
      0,
    );
    assert.ok(frameControls >= 2, "animated GIF lost its animation frames");
  }
  await eventually(
    `${extension} public cache`,
    async () => {
      const response = await timedFetch(publicURL);
      await response.arrayBuffer();
      return {
        status: response.status,
        cacheStatus: response.headers.get("cf-cache-status"),
      };
    },
    (value) => value.status === 200 && value.cacheStatus === "HIT",
    30_000,
  );
  return upload;
}

async function sendProjection(identifier, position, references) {
  const sourceIdentifier = `remote-smoke-${identifier}`;
  const response = await timedFetch(
    `https://api.cloudflare.com/client/v4/accounts/` +
      `${process.env.CLOUDFLARE_ACCOUNT_ID}/queues/` +
      `${process.env.MEDIA_REFERENCE_QUEUE_ID}/messages`,
    {
      method: "POST",
      headers: {
        authorization: `Bearer ${process.env.CLOUDFLARE_API_TOKEN}`,
        "content-type": "application/json",
      },
      body: JSON.stringify({
        body: {
          eventIdentifier: `remote-smoke-${identifier}-${position}`,
          sourcePosition: String(position),
          sourceKind: "article",
          sourceIdentifier,
          referencedImages: references,
          occurredAt: new Date().toISOString(),
        },
        content_type: "json",
      }),
    },
  );
  const result = await response.json();
  assert.ok(
    response.ok && result.success,
    `projection enqueue failed: ${response.status} ${JSON.stringify(result)}`,
  );
  await eventually(
    `projection ${position}`,
    () => driverJSON(`/state/${identifier}`),
    (state) => state.usageCount === references.length,
  );
}

async function invokeRetention() {
  const response = await timedFetch(
    `http://127.0.0.1:${retentionPort}/__scheduled?cron=0+3+*+*+*`,
  );
  assert.ok(response.ok, `scheduled retention failed: ${response.status}`);
}

async function purgeURLs(urls) {
  if (urls.length === 0) return;
  const endpoint =
    "https://api.cloudflare.com/client/v4/zones/" +
    `${process.env.CLOUDFLARE_ZONE_IDENTIFIER}/purge_cache`;
  const response = await timedFetch(
    endpoint,
    {
      method: "POST",
      headers: {
        authorization: `Bearer ${process.env.CLOUDFLARE_CACHE_PURGE_TOKEN}`,
        "content-type": "application/json",
      },
      body: JSON.stringify({ files: urls }),
    },
  );
  assert.ok(response.ok, `cache cleanup failed: ${response.status}`);
}

async function main() {
  const configs = await writeConfigurations();
  const children = [];
  const uploaded = [];
  try {
    const driver = startWrangler(configs.driverPath, driverPort);
    children.push(driver);
    await waitForReady(`${driverURL}/ready`);

    for (const fixture of fixtures) {
      await uploadFixture(...fixture, uploaded);
    }

    const availability = await driverJSON("/api/images/availability", {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        images: uploaded.map(({ identifier }) => identifier),
      }),
    });
    assert.deepEqual(
      new Set(availability.available),
      new Set(uploaded.map(({ identifier }) => identifier)),
    );

    const [unreferenced, referenced] = uploaded;
    await sendProjection(referenced.identifier, 1, [referenced.identifier]);
    await driverJSON("/age", {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        identifiers: [unreferenced.identifier, referenced.identifier],
        timestamp: "2020-01-01T00:00:00Z",
      }),
    });

    const retention = startWrangler(configs.retentionPath, retentionPort, [
      "--test-scheduled",
    ]);
    children.push(retention);
    await waitForListening(`http://127.0.0.1:${retentionPort}/`);
    await invokeRetention();
    await eventually(
      "unreferenced retention",
      () => driverJSON(`/state/${unreferenced.identifier}`),
      (state) => state.image === null && state.finalExists === false,
    );
    const protectedState = await driverJSON(`/state/${referenced.identifier}`);
    assert.equal(protectedState.image.state, "available");
    assert.equal(protectedState.finalExists, true);

    await sendProjection(referenced.identifier, 2, []);
    await invokeRetention();
    await eventually(
      "released reference retention",
      () => driverJSON(`/state/${referenced.identifier}`),
      (state) => state.image === null && state.finalExists === false,
    );
    for (const image of [unreferenced, referenced]) {
      const response = await timedFetch(image.publicURL, { cache: "no-store" });
      assert.equal(response.status, 404);
      assert.notEqual(response.headers.get("cf-cache-status"), "HIT");
    }
    console.log("Media remote smoke passed for five formats and retention.");
  } finally {
    try {
      if (uploaded.length > 0) {
        await driverJSON("/cleanup", {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify({ identifiers: uploaded.map(({ identifier }) => identifier) }),
        });
        await purgeURLs(uploaded.map(({ publicURL }) => publicURL));
      }
    } finally {
      for (const child of children) stopWrangler(child);
      await Promise.allSettled([
        rm(configs.driverPath, { force: true }),
        rm(configs.retentionPath, { force: true }),
      ]);
    }
  }
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
