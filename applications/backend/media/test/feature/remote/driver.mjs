function json(value, status = 200) {
  return Response.json(value, { status });
}

function checksumText(value) {
  if (value === undefined) return null;
  return btoa(String.fromCharCode(...new Uint8Array(value)));
}

async function forwardToAPI(request, environment, path) {
  const headers = new Headers(request.headers);
  headers.set("X-Hut-Actor", "media-remote-smoke");
  headers.delete("X-Correlation-Identifier");
  return environment.MEDIA_API_WORKER.fetch(
    new Request(`https://media.internal${path}`, {
      method: request.method,
      headers,
      body: request.body,
      redirect: "manual",
    }),
  );
}

async function imageState(environment, identifier) {
  const image = await environment.MEDIA_DATABASE.prepare(
    "SELECT state,inspection_started_at AS inspectionStartedAt," +
      "available_at AS availableAt,rejected_at AS rejectedAt," +
      "rejection_code AS rejectionCode FROM images WHERE identifier=?",
  )
    .bind(identifier)
    .first();
  const attempt = await environment.MEDIA_DATABASE.prepare(
    "SELECT identifier,temporary_object_key AS temporaryObjectKey," +
      "uploaded_at AS uploadedAt FROM upload_attempts " +
      "WHERE image_identifier=? ORDER BY requested_at DESC LIMIT 1",
  )
    .bind(identifier)
    .first();
  const inspection = await environment.MEDIA_DATABASE.prepare(
    "SELECT inspected_at AS inspectedAt FROM image_inspections " +
      "WHERE image_identifier=? ORDER BY inspected_at DESC LIMIT 1",
  )
    .bind(identifier)
    .first();
  const finalKey = `images/${identifier}`;
  const temporary = attempt?.temporaryObjectKey
    ? await environment.MEDIA_TMP_UPLOADS.head(attempt.temporaryObjectKey)
    : null;
  const final = await environment.MEDIA_ASSETS.head(finalKey);
  const usage = await environment.MEDIA_DATABASE.prepare(
    "SELECT COUNT(*) AS count FROM image_usages WHERE image_identifier=?",
  )
    .bind(identifier)
    .first();
  return json({
    image,
    attempt,
    inspection,
    temporaryExists: temporary !== null,
    finalExists: final !== null,
    finalSize: final?.size ?? null,
    finalSha256: checksumText(final?.checksums?.sha256),
    usageCount: Number(usage?.count ?? 0),
  });
}

async function ageImages(request, environment) {
  const { identifiers, timestamp } = await request.json();
  for (const identifier of identifiers) {
    await environment.MEDIA_DATABASE.prepare(
      "UPDATE images SET available_at=?,updated_at=? " +
        "WHERE identifier=? AND state='available'",
    )
      .bind(timestamp, timestamp, identifier)
      .run();
  }
  return json({ updated: identifiers.length });
}

async function cleanup(request, environment) {
  const { identifiers } = await request.json();
  for (const identifier of identifiers) {
    const attempts = await environment.MEDIA_DATABASE.prepare(
      "SELECT temporary_object_key AS objectKey FROM upload_attempts " +
        "WHERE image_identifier=?",
    )
      .bind(identifier)
      .all();
    for (const row of attempts.results) {
      await environment.MEDIA_TMP_UPLOADS.delete(row.objectKey);
    }
    await environment.MEDIA_ASSETS.delete(`images/${identifier}`);
    await environment.MEDIA_DATABASE.prepare(
      "DELETE FROM retention_claims WHERE image_identifier=?",
    )
      .bind(identifier)
      .run();
    await environment.MEDIA_DATABASE.prepare(
      "DELETE FROM image_usages WHERE image_identifier=?",
    )
      .bind(identifier)
      .run();
    await environment.MEDIA_DATABASE.prepare(
      "DELETE FROM images WHERE identifier=?",
    )
      .bind(identifier)
      .run();
  }
  await environment.MEDIA_DATABASE.prepare(
    "DELETE FROM projection_positions WHERE stream_name LIKE 'article:remote-smoke-%'",
  ).run();
  return json({ deleted: identifiers.length });
}

export default {
  async fetch(request, environment) {
    const url = new URL(request.url);
    if (url.pathname === "/ready") return json({ ready: true });
    if (url.pathname.startsWith("/api/")) {
      return forwardToAPI(request, environment, url.pathname.slice(4));
    }
    if (request.method === "GET" && url.pathname.startsWith("/state/")) {
      return imageState(environment, url.pathname.slice("/state/".length));
    }
    if (request.method === "POST" && url.pathname === "/age") {
      return ageImages(request, environment);
    }
    if (request.method === "POST" && url.pathname === "/cleanup") {
      return cleanup(request, environment);
    }
    return json({ error: "not_found" }, 404);
  },
};
