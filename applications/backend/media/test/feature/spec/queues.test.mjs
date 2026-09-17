import assert from "node:assert/strict";
import test from "node:test";

const baseURL = process.env.FEATURE_BASE_URL;

async function eventually(path, predicate) {
  const deadline = Date.now() + 30_000;
  let last;
  while (Date.now() < deadline) {
    const response = await fetch(`${baseURL}${path}`);
    assert.equal(response.status, 200);

    last = await response.json();

    if (predicate(last)) return last;
    await new Promise((resolve) => setTimeout(resolve, 250));
  }
  assert.fail(`condition was not met for ${path}: ${JSON.stringify(last)}`);
}

test("inspection Queue normalizes a PNG through Images", async () => {
  const seeded = await fetch(`${baseURL}/feature/inspection`, {
    method: "POST",
  });
  assert.equal(seeded.status, 202);

  const result = await eventually(
    "/feature/inspection",
    (value) => value.state === "available",
  );

  assert.equal(result.temporaryExists, false);
  assert.equal(result.assetExists, true);
});

test("reference Queue projects an article image usage into D1", async () => {
  const seeded = await fetch(`${baseURL}/feature/projection`, {
    method: "POST",
  });
  assert.equal(seeded.status, 202);

  const result = await eventually(
    "/feature/projection",
    (value) => value.imageIdentifier !== null,
  );

  assert.equal(result.imageIdentifier, "01J00000000000000000000003");
});
