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

test("legacy aggregates are migrated before admin queries", async () => {
  const identifier = "01ARZ3NDEKTSV4RRFFQ69G5FB9";
  const response = await fetch(apiRoute(`/admin/articles/${identifier}`), {
    headers: { "X-Hut-Actor": "editor" },
  });
  assert.equal(response.status, 200, await response.clone().text());
  const article = await response.json();
  assert.equal(article.identifier, identifier);
  assert.equal(article.phase, "unvalidated");
  const page = await fetch(apiRoute("/admin/articles?status=unvalidated"), {
    headers: { "X-Hut-Actor": "editor" },
  });
  assert.equal(page.status, 200, await page.clone().text());
  assert.ok((await page.json()).articles.some((item) => item.identifier === identifier));
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

test("failed generation can be replaced by a new request", async () => {
  const headers = { "X-Hut-Actor": "editor", "content-type": "application/json" };
  const createdResponse = await fetch(apiRoute("/admin/articles"), {
    method: "POST",
    headers,
    body: JSON.stringify({
      title: "Generation recovery",
      body: "# Recovery\n\nThe failed request must not block this article.",
      slug: "article-generation-recovery-feature",
      tags: [],
    }),
  });
  assert.equal(createdResponse.status, 201, await createdResponse.clone().text());
  const { identifier } = await createdResponse.json();
  const proofread = await fetch(apiRoute(`/admin/articles/${identifier}/proofreading`), {
    method: "POST",
    headers,
  });
  assert.equal(proofread.status, 200, await proofread.clone().text());
  const regenerationRoute = `/admin/articles/${identifier}/excerpt-generation-requests`;
  const first = await fetch(apiRoute(regenerationRoute), { method: "POST", headers });
  assert.equal(first.status, 200, await first.clone().text());
  const { requestIdentifier: firstRequest } = await first.json();

  const abandoned = await fetch(route("/internal/excerpt-generation/abandon"), {
    method: "POST",
    headers,
    body: JSON.stringify({
      identifier: firstRequest,
      article: identifier,
      expectedRevision: 2,
    }),
  });
  assert.equal(abandoned.status, 200, await abandoned.clone().text());

  const second = await fetch(apiRoute(regenerationRoute), { method: "POST", headers });
  assert.equal(second.status, 200, await second.clone().text());
  const { requestIdentifier: secondRequest } = await second.json();
  assert.notEqual(secondRequest, firstRequest);

  const oldClaim = await fetch(route(
    "/internal/excerpt-generation/claim" +
    `?article=${identifier}&request=${firstRequest}&expectedRevision=2`,
  ));
  assert.equal(oldClaim.status, 204, await oldClaim.clone().text());
  const newClaim = await fetch(route(
    "/internal/excerpt-generation/claim" +
    `?article=${identifier}&request=${secondRequest}&expectedRevision=2`,
  ));
  assert.equal(newClaim.status, 200, await newClaim.clone().text());
});

test("article creation rejects non-ULID tag identifiers", async () => {
  const response = await fetch(apiRoute("/admin/articles"), {
    method: "POST",
    headers: { "X-Hut-Actor": "editor", "content-type": "application/json" },
    body: JSON.stringify({
      title: "Invalid tag",
      body: "Draft body",
      slug: "invalid-tag-feature",
      tags: ["haskell"],
    }),
  });
  assert.equal(response.status, 400, await response.clone().text());
  assert.equal(response.headers.get("X-Article-Error-Code"), "invalid_article");
});

test("article lifecycle is available through the admin and reader APIs", async () => {
  const headers = { "X-Hut-Actor": "editor", "content-type": "application/json" };
  const slug = "article-lifecycle-feature";
  const tag = "01ARZ3NDEKTSV4RRFFQ69G5FAY";
  const createdResponse = await fetch(apiRoute("/admin/articles"), {
    method: "POST",
    headers,
    body: JSON.stringify({ title: "Lifecycle", body: "# Lifecycle\n\nA complete article.", slug, tags: [tag] }),
  });
  assert.equal(createdResponse.status, 201, await createdResponse.clone().text());
  const created = await createdResponse.json();
  assert.match(created.identifier, /^[0-9A-HJKMNP-TV-Z]{26}$/);
  assert.equal(created.phase, "unvalidated");
  assert.equal(created.slug, slug);
  const identifier = created.identifier;

  const adminPage = await fetch(apiRoute("/admin/articles?status=unvalidated"), {
    headers,
  });
  assert.equal(adminPage.status, 200, await adminPage.clone().text());
  assert.ok((await adminPage.json()).articles.some((article) => article.identifier === identifier));

  const availability = await fetch(
    apiRoute(`/admin/articles/${identifier}/slug-availability?slug=${slug}`),
    { headers },
  );
  assert.equal(availability.status, 200);
  assert.deepEqual(await availability.json(), { available: true });

  const amendedResponse = await fetch(apiRoute(`/admin/articles/${identifier}/draft`), {
    method: "PUT",
    headers,
    body: JSON.stringify({ title: "Lifecycle", body: "# Lifecycle\n\nUpdated article.", slug, tags: [tag] }),
  });
  assert.equal(amendedResponse.status, 200, await amendedResponse.clone().text());
  assert.match((await amendedResponse.json()).body, /Updated article/);

  const proofread = await fetch(apiRoute(`/admin/articles/${identifier}/proofreading`), {
    method: "POST",
    headers,
  });
  assert.equal(proofread.status, 200, await proofread.clone().text());

  const regeneration = await fetch(
    apiRoute(`/admin/articles/${identifier}/excerpt-generation-requests`),
    { method: "POST", headers },
  );
  assert.equal(regeneration.status, 200, await regeneration.clone().text());
  const { requestIdentifier } = await regeneration.json();
  const claim = await fetch(route(
    `/internal/excerpt-generation/claim?article=${identifier}` +
    `&request=${requestIdentifier}&expectedRevision=3`,
  ));
  assert.equal(claim.status, 200, await claim.clone().text());
  const claimed = await claim.json();
  assert.equal(claimed.content.slug, slug);
  const generated = await fetch(route("/internal/excerpt-generation/complete"), {
    method: "POST",
    headers,
    body: JSON.stringify({
      identifier: "01ARZ3NDEKTSV4RRFFQ69G5FC0",
      occurredAt: new Date().toISOString(),
      actor: "editor",
      correlation: "01ARZ3NDEKTSV4RRFFQ69G5FC1",
      causation: null,
      event: {
        request: requestIdentifier,
        article: identifier,
        expectedRevision: 3,
        excerpt: "An introduction to the lifecycle of this article.",
      },
    }),
  });
  assert.equal(generated.status, 200, await generated.clone().text());

  const revised = await fetch(apiRoute(`/admin/articles/${identifier}/excerpt`), {
    method: "PATCH",
    headers,
    body: JSON.stringify({ excerpt: "An edited introduction to the article." }),
  });
  assert.equal(revised.status, 200, await revised.clone().text());
  assert.equal((await revised.json()).phase, "ready");

  const published = await fetch(apiRoute(`/admin/articles/${identifier}/publication`), {
    method: "POST",
    headers,
  });
  assert.equal(published.status, 200, await published.clone().text());
  assert.equal((await published.json()).phase, "published");
  const reader = await fetch(apiRoute(`/articles/${slug}`));
  assert.equal(reader.status, 200, await reader.clone().text());
  const readerArticle = await reader.json();
  assert.equal(readerArticle.identifier, identifier);
  assert.deepEqual(readerArticle.tags, [tag]);
  const readerPage = await fetch(apiRoute("/articles"));
  assert.equal(readerPage.status, 200, await readerPage.clone().text());
  assert.ok((await readerPage.json()).articles.some((article) => article.identifier === identifier));

  const takenDown = await fetch(apiRoute(`/admin/articles/${identifier}/publication`), {
    method: "DELETE",
    headers,
  });
  assert.equal(takenDown.status, 200, await takenDown.clone().text());
  assert.equal((await takenDown.json()).phase, "private");
  assert.equal((await fetch(apiRoute(`/articles/${slug}`))).status, 404);

  const resumed = await fetch(apiRoute(`/admin/articles/${identifier}/publication-resumptions`), {
    method: "POST",
    headers,
  });
  assert.equal(resumed.status, 200, await resumed.clone().text());
  assert.equal((await resumed.json()).phase, "ready");
  const republished = await fetch(apiRoute(`/admin/articles/${identifier}/publication`), {
    method: "POST",
    headers,
  });
  assert.equal(republished.status, 200, await republished.clone().text());
  assert.equal((await republished.json()).phase, "published");

  const finalTakeDown = await fetch(apiRoute(`/admin/articles/${identifier}/publication`), {
    method: "DELETE",
    headers,
  });
  assert.equal(finalTakeDown.status, 200, await finalTakeDown.clone().text());

  const discarded = await fetch(apiRoute(`/admin/articles/${identifier}`), {
    method: "DELETE",
    headers,
  });
  assert.equal(discarded.status, 200, await discarded.clone().text());
  assert.deepEqual(await discarded.json(), { article: identifier });
  assert.equal((await fetch(apiRoute(`/articles/${slug}`))).status, 404);

  let projections = [];
  let received = [];
  for (let attempt = 0; attempt < 5; attempt += 1) {
    const response = await fetch(`${base}/media-projections`);
    assert.equal(response.status, 200);
    received = await response.json();
    projections = received.filter(
      (message) => message.sourceIdentifier === identifier,
    );
    if (projections.length >= 3) break;
    await new Promise((resolve) => setTimeout(resolve, 1000));
  }
  assert.equal(projections.length, 3, JSON.stringify(received));
  assert.ok(projections.every((message) => message.sourceKind === "article"));
  assert.ok(projections.every((message) => message.referencedImages.length === 0));
  assert.equal(new Set(projections.map((message) => message.sourcePosition)).size, 3);
  assert.equal(new Set(projections.map((message) => message.eventIdentifier)).size, 3);
});
