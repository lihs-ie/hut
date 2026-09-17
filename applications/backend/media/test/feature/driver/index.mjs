const identifiers = {
  apiMissingImage: "01J00000000000000000000000",
  inspectionImage: "01J00000000000000000000001",
  inspectionAttempt: "01J00000000000000000000002",
  projectionImage: "01J00000000000000000000003",
};

const png = Uint8Array.from(
  atob(
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk" +
      "+A8AAQUBAScY42YAAAAASUVORK5CYII=",
  ),
  (character) => character.charCodeAt(0),
);

function json(body, status = 200) {
  return Response.json(body, { status });
}

async function resourceProbe(environment) {
  const database = await environment.MEDIA_DATABASE.prepare(
    "SELECT COUNT(*) AS count FROM images",
  ).first();
  const key = "feature/probe";
  await environment.MEDIA_TMP_UPLOADS.put(key, "probe");
  const stored = await environment.MEDIA_TMP_UPLOADS.get(key);
  const value = stored === null ? null : await stored.text();
  await environment.MEDIA_TMP_UPLOADS.delete(key);

  return json({ databaseCount: database.count, r2Value: value });
}

async function apiProbe(request, environment) {
  const url = new URL(request.url);
  const imageIdentifier = identifiers.apiMissingImage;
  const response = await environment.MEDIA_API_WORKER.fetch(
    new Request(`https://media.feature/images/${imageIdentifier}${url.search}`, {
      headers: {
        "X-Hut-Actor": "feature-admin-worker",
        "X-Correlation-Identifier": "01J00000000000000000000004",
      },
    }),
  );
  return json({
    status: response.status,
    correlation: response.headers.get("X-Correlation-Identifier"),
    body: await response.json(),
  });
}

async function seedInspection(environment) {
  const now = "2026-09-14T00:00:00Z";
  const expires = "2026-09-14T01:00:00Z";
  const objectKey =
    `${identifiers.inspectionImage}/${identifiers.inspectionAttempt}`;
  const digest = await crypto.subtle.digest("SHA-256", png);
  const sha256 = Array.from(new Uint8Array(digest), (byte) =>
    byte.toString(16).padStart(2, "0"),
  ).join("");

  await environment.MEDIA_DATABASE.batch([
    environment.MEDIA_DATABASE.prepare(
      "INSERT INTO images " +
        "(identifier,state,current_upload_attempt_identifier,requested_at," +
        "created_at,updated_at) VALUES (?,'awaiting_upload',?,?,?,?)",
    ).bind(
      identifiers.inspectionImage,
      identifiers.inspectionAttempt,
      now,
      now,
      now,
    ),
    environment.MEDIA_DATABASE.prepare(
      "INSERT INTO upload_attempts " +
        "(identifier,image_identifier,temporary_object_key," +
        "declared_content_type,declared_byte_size,declared_sha256," +
        "requested_at,expires_at) VALUES (?,?,?,?,?,?,?,?)",
    ).bind(
      identifiers.inspectionAttempt,
      identifiers.inspectionImage,
      objectKey,
      "image/png",
      png.byteLength,
      sha256,
      now,
      expires,
    ),
  ]);
  await environment.MEDIA_TMP_UPLOADS.put(objectKey, png, {
    httpMetadata: { contentType: "image/png" },
    sha256: digest,
  });
  await environment.MEDIA_INSPECTION_QUEUE.send({ data: { key: objectKey } });
  return json({ imageIdentifier: identifiers.inspectionImage }, 202);
}

async function inspectionStatus(environment) {
  const image = await environment.MEDIA_DATABASE.prepare(
    "SELECT state FROM images WHERE identifier=?",
  )
    .bind(identifiers.inspectionImage)
    .first();
  const temporaryKey =
    `${identifiers.inspectionImage}/${identifiers.inspectionAttempt}`;
  const finalKey = `images/${identifiers.inspectionImage}`;
  return json({
    state: image?.state ?? null,
    temporaryExists:
      (await environment.MEDIA_TMP_UPLOADS.head(temporaryKey)) !== null,
    assetExists: (await environment.MEDIA_ASSETS.head(finalKey)) !== null,
  });
}

async function seedProjection(environment) {
  const now = "2026-09-14T00:00:00Z";
  await environment.MEDIA_DATABASE.prepare(
    "INSERT INTO images " +
      "(identifier,state,available_at,created_at,updated_at) " +
      "VALUES (?,'available',?,?,?)",
  )
    .bind(identifiers.projectionImage, now, now, now)
    .run();
  await environment.MEDIA_REFERENCE_QUEUE.send({
    eventIdentifier: "projection-event-1",
    sourcePosition: "1",
    sourceKind: "article",
    sourceIdentifier: "feature-article",
    referencedImages: [identifiers.projectionImage],
    occurredAt: now,
  });
  return json({ sourceIdentifier: "feature-article" }, 202);
}

async function projectionStatus(environment) {
  const usage = await environment.MEDIA_DATABASE.prepare(
    "SELECT image_identifier AS imageIdentifier FROM image_usages " +
      "WHERE source_kind='article' AND source_identifier='feature-article'",
  ).first();
  return json({ imageIdentifier: usage?.imageIdentifier ?? null });
}

export default {
  async fetch(request, environment) {
    const url = new URL(request.url);
    const route = `${request.method} ${url.pathname}`;
    switch (route) {
      case "GET /ready":
        return json({ ready: true });
      case "GET /feature/resources":
        return resourceProbe(environment);
      case "GET /feature/api":
        return apiProbe(request, environment);
      case "POST /feature/inspection":
        return seedInspection(environment);
      case "GET /feature/inspection":
        return inspectionStatus(environment);
      case "POST /feature/projection":
        return seedProjection(environment);
      case "GET /feature/projection":
        return projectionStatus(environment);
      default:
        return json({ error: "not_found" }, 404);
    }
  },
};
