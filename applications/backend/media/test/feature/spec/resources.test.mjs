import assert from "node:assert/strict";
import test from "node:test";

const baseURL = process.env.FEATURE_BASE_URL;

test("local D1 and R2 bindings persist real data", async () => {
  const response = await fetch(`${baseURL}/feature/resources`);
  assert.equal(response.status, 200);

  const body = await response.json();
  assert.equal(typeof body.databaseCount, "number");
  assert.equal(body.r2Value, "probe");
});
