import assert from "node:assert/strict";
import { test } from "node:test";

const base = process.env.FEATURE_BASE_URL;
const article = "01ARZ3NDEKTSV4RRFFQ69G5FAV";
const request = "01ARZ3NDEKTSV4RRFFQ69G5FAW";

function route(path) {
  return `${base}/article-do?path=${encodeURIComponent(path)}`;
}

function apiRoute(path) {
  return `${base}/article-api?path=${encodeURIComponent(path)}`;
}

test("unknown generation request is acknowledged by the real Article DO", async () => {
  const path = "/internal/excerpt-generation/claim" +
    `?article=${article}&request=${request}&expectedRevision=1`;
  const response = await fetch(route(path));
  assert.equal(response.status, 204);
  assert.equal(await response.text(), "");
});

test("malformed internal requests are rejected", async () => {
  const missingClaim = await fetch(route("/internal/excerpt-generation/claim"));
  assert.equal(missingClaim.status, 400);

  const badCompletion = await fetch(route("/internal/excerpt-generation/complete"), {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: "not-json",
  });
  assert.equal(badCompletion.status, 400);

  const unknown = await fetch(route("/internal/unknown"));
  assert.equal(unknown.status, 404);
});

test("admin proofread confirms available images through Media and requests generation", async () => {
  const identifier = "01ARZ3NDEKTSV4RRFFQ69G5FB0";
  const response = await fetch(apiRoute(`/admin/articles/${identifier}/proofreading`), {
    method: "POST",
    headers: { "X-Hut-Actor": "editor" },
  });
  assert.equal(response.status, 200, await response.clone().text());
  assert.deepEqual(await response.json(), { article: identifier, phase: "proofreaded" });
  assert.ok(response.headers.get("X-Correlation-Identifier"));
});

test("admin proofread works without image references", async () => {
  const identifier = "01ARZ3NDEKTSV4RRFFQ69G5FB2";
  const response = await fetch(apiRoute(`/admin/articles/${identifier}/proofreading`), {
    method: "POST",
    headers: { "X-Hut-Actor": "editor" },
  });
  assert.equal(response.status, 200, await response.clone().text());
});

test("admin proofread rejects images still under inspection", async () => {
  const identifier = "01ARZ3NDEKTSV4RRFFQ69G5FB1";
  const response = await fetch(apiRoute(`/admin/articles/${identifier}/proofreading`), {
    method: "POST",
    headers: { "X-Hut-Actor": "editor" },
  });
  assert.equal(response.status, 400);
  assert.equal(response.headers.get("X-Article-Error-Code"), "invalid_article");
});

test("admin regeneration route records a request for a proofreaded article", async () => {
  const identifier = "01ARZ3NDEKTSV4RRFFQ69G5FB0";
  const response = await fetch(apiRoute(`/admin/articles/${identifier}/excerpt-generation-requests`), {
    method: "POST",
    headers: { "X-Hut-Actor": "editor" },
  });
  assert.equal(response.status, 200, await response.clone().text());
  const payload = await response.json();
  assert.equal(payload.article, identifier);
  assert.match(payload.requestIdentifier, /^[0-9A-HJKMNP-TV-Z]{26}$/);
});
