import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";
import { createTestHarness } from "wrangler";

const root = process.cwd();
const imageIdentifier = "01J00000000000000000000004";
const uploadAttempt = "01J00000000000000000000005";
const objectKey = `${imageIdentifier}/${uploadAttempt}`;

test("Scheduled retention removes an expired image from D1 and temporary R2", async () => {
  const server = createTestHarness({
    root,
    workers: [
      {
        configPath: path.join(
          root,
          "test/feature/wrangler/retention.jsonc",
        ),
      },
    ],
  });
  try {
    await server.listen();
    const worker = server.getWorker("hut-media-retention-worker-feature");
    await worker.applyD1Migrations("MEDIA_DATABASE");
    const environment = await worker.getEnv();
    const requestedAt = "2020-01-01T00:00:00Z";
    const expiresAt = "2020-01-01T01:00:00Z";
    await environment.MEDIA_DATABASE.batch([
      environment.MEDIA_DATABASE.prepare(
        "INSERT INTO images " +
          "(identifier,state,current_upload_attempt_identifier,requested_at," +
          "created_at,updated_at) VALUES (?,'awaiting_upload',?,?,?,?)",
      ).bind(
        imageIdentifier,
        uploadAttempt,
        requestedAt,
        requestedAt,
        requestedAt,
      ),
      environment.MEDIA_DATABASE.prepare(
        "INSERT INTO upload_attempts " +
          "(identifier,image_identifier,temporary_object_key," +
          "declared_content_type,declared_byte_size,declared_sha256," +
          "requested_at,expires_at) VALUES (?,?,?,?,?,?,?,?)",
      ).bind(
        uploadAttempt,
        imageIdentifier,
        objectKey,
        "image/png",
        1,
        "0".repeat(64),
        requestedAt,
        expiresAt,
      ),
    ]);
    await environment.MEDIA_TMP_UPLOADS.put(objectKey, "retained");

    await worker.scheduled({
      cron: "0 3 * * *",
      scheduledTime: new Date("2026-09-14T00:00:00Z"),
    });

    const image = await environment.MEDIA_DATABASE.prepare(
      "SELECT identifier FROM images WHERE identifier=?",
    )
      .bind(imageIdentifier)
      .first();
    const object = await environment.MEDIA_TMP_UPLOADS.head(objectKey);
    assert.equal(image, null);
    assert.equal(object, null);
  } finally {
    await server.close();
  }
});
