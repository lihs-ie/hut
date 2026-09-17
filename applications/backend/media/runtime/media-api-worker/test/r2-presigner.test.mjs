import assert from "node:assert/strict";
import test from "node:test";

import { createMediaApiAdapters } from "../src/r2-presigner.ts";

const environment = {
  R2_S3_ACCESS_KEY_ID: "test-access-key",
  R2_S3_SECRET_ACCESS_KEY: "test-secret-key",
  R2_ACCOUNT_IDENTIFIER: "test-account",
  MEDIA_TMP_UPLOADS_BUCKET_NAME: "hut-media-tmp-uploads-stg",
};

test("creates the private JSFFI presign function", () => {
  const adapters = createMediaApiAdapters(environment);

  assert.equal(typeof adapters.presignR2Put, "function");
});

test("signs a PUT URL for the configured temporary bucket", async () => {
  const binding = createMediaApiAdapters(environment);
  const signed = await binding.presignR2Put({
    objectKey: "tmp/test key",
    contentType: "image/png",
    expiresInSeconds: 60,
  });

  assert.equal(
    signed.pathname,
    "/hut-media-tmp-uploads-stg/tmp/test%20key",
  );
  assert.equal(signed.searchParams.get("X-Amz-Expires"), "60");
  assert.equal(signed.searchParams.get("X-Amz-Algorithm"), "AWS4-HMAC-SHA256");
});

test("rejects object keys that URL normalization could escape", async () => {
  const binding = createMediaApiAdapters(environment);

  for (const objectKey of ["", "../assets/key", "tmp/../key", "tmp//key"]) {
    await assert.rejects(
      binding.presignR2Put({
        objectKey,
        contentType: "image/png",
        expiresInSeconds: 60,
      }),
      TypeError,
    );
  }
});

test("rejects invalid expiry and content type values", async () => {
  const binding = createMediaApiAdapters(environment);

  await assert.rejects(
    binding.presignR2Put({
      objectKey: "tmp/key",
      contentType: " ",
      expiresInSeconds: 60,
    }),
    TypeError,
  );

  for (const expiresInSeconds of [0, 1.5, 604_801]) {
    await assert.rejects(
      binding.presignR2Put({
        objectKey: "tmp/key",
        contentType: "image/png",
        expiresInSeconds,
      }),
      RangeError,
    );
  }
});
