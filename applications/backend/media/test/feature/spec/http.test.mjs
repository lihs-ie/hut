import assert from "node:assert/strict";
import test from "node:test";

const baseURL = process.env.FEATURE_BASE_URL;

test("API Worker accepts actor metadata through its service binding", async () => {
  const response = await fetch(`${baseURL}/feature/api`);
  assert.equal(response.status, 200);

  const result = await response.json();
  assert.equal(result.status, 404);
  assert.equal(result.correlation, "01J00000000000000000000004");
  assert.equal(result.body.code, "image_not_found");
});

test("availability API reads available images from D1 in one request", async () => {
  const response = await fetch(`${baseURL}/feature/availability`, {
    method: "POST",
  });
  assert.equal(response.status, 200);

  const result = await response.json();
  assert.equal(result.status, 200);
  assert.equal(result.correlation, "01J00000000000000000000004");
  assert.deepEqual(result.body.available, ["01J00000000000000000000005"]);
});
