PRAGMA foreign_keys = ON;

-- Times are UTC RFC 3339 text emitted by the application. Identifiers are ULID text.
CREATE TABLE IF NOT EXISTS upload_attempts (
  identifier TEXT PRIMARY KEY NOT NULL,
  image_identifier TEXT NOT NULL,
  temporary_object_key TEXT NOT NULL UNIQUE,
  declared_content_type TEXT NOT NULL,
  declared_byte_size INTEGER NOT NULL CHECK (declared_byte_size > 0),
  declared_sha256 TEXT NOT NULL CHECK (length(declared_sha256) = 64),
  requested_at TEXT NOT NULL,
  expires_at TEXT NOT NULL,
  uploaded_at TEXT,
  superseded_at TEXT,
  FOREIGN KEY (image_identifier) REFERENCES images(identifier) ON DELETE CASCADE,
  CHECK (expires_at > requested_at),
  CHECK (uploaded_at IS NULL OR uploaded_at >= requested_at),
  CHECK (superseded_at IS NULL OR superseded_at >= requested_at)
);

-- The aggregate state is deliberately separate from upload and inspection metadata.
CREATE TABLE IF NOT EXISTS images (
  identifier TEXT PRIMARY KEY NOT NULL,
  state TEXT NOT NULL CHECK (state IN ('awaiting_upload', 'inspecting', 'available', 'rejected')),
  current_upload_attempt_identifier TEXT UNIQUE,
  requested_at TEXT,
  inspection_started_at TEXT,
  available_at TEXT,
  rejected_at TEXT,
  rejection_code TEXT,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  FOREIGN KEY (current_upload_attempt_identifier)
    REFERENCES upload_attempts(identifier)
    DEFERRABLE INITIALLY DEFERRED,
  CHECK (
    (state = 'awaiting_upload'
      AND current_upload_attempt_identifier IS NOT NULL
      AND requested_at IS NOT NULL
      AND inspection_started_at IS NULL
      AND available_at IS NULL
      AND rejected_at IS NULL
      AND rejection_code IS NULL)
    OR
    (state = 'inspecting'
      AND current_upload_attempt_identifier IS NOT NULL
      AND requested_at IS NOT NULL
      AND inspection_started_at IS NOT NULL
      AND available_at IS NULL
      AND rejected_at IS NULL
      AND rejection_code IS NULL)
    OR
    (state = 'available'
      AND current_upload_attempt_identifier IS NULL
      AND requested_at IS NULL
      AND inspection_started_at IS NULL
      AND available_at IS NOT NULL
      AND rejected_at IS NULL
      AND rejection_code IS NULL)
    OR
    (state = 'rejected'
      AND current_upload_attempt_identifier IS NULL
      AND requested_at IS NULL
      AND inspection_started_at IS NULL
      AND available_at IS NULL
      AND rejected_at IS NOT NULL
      AND rejection_code IS NOT NULL)
  )
);

CREATE TABLE IF NOT EXISTS image_inspections (
  identifier TEXT PRIMARY KEY NOT NULL,
  image_identifier TEXT NOT NULL,
  upload_attempt_identifier TEXT NOT NULL,
  source_format TEXT,
  width INTEGER CHECK (width IS NULL OR width > 0),
  height INTEGER CHECK (height IS NULL OR height > 0),
  byte_size INTEGER CHECK (byte_size IS NULL OR byte_size > 0),
  pixel_count INTEGER CHECK (pixel_count IS NULL OR pixel_count > 0),
  final_object_key TEXT UNIQUE,
  inspected_at TEXT NOT NULL,
  FOREIGN KEY (image_identifier) REFERENCES images(identifier) ON DELETE CASCADE,
  FOREIGN KEY (upload_attempt_identifier) REFERENCES upload_attempts(identifier) ON DELETE CASCADE,
  UNIQUE (image_identifier, upload_attempt_identifier),
  CHECK (
    (final_object_key IS NULL
      AND source_format IS NULL
      AND width IS NULL
      AND height IS NULL
      AND byte_size IS NULL
      AND pixel_count IS NULL)
    OR
    (final_object_key IS NOT NULL
      AND source_format IS NOT NULL
      AND width IS NOT NULL
      AND height IS NOT NULL
      AND byte_size IS NOT NULL
      AND pixel_count IS NOT NULL)
  )
);

CREATE TABLE IF NOT EXISTS inspection_failures (
  identifier TEXT PRIMARY KEY NOT NULL,
  image_identifier TEXT NOT NULL,
  upload_attempt_identifier TEXT NOT NULL,
  failure_code TEXT NOT NULL,
  detail TEXT,
  failed_at TEXT NOT NULL,
  FOREIGN KEY (image_identifier) REFERENCES images(identifier) ON DELETE CASCADE,
  FOREIGN KEY (upload_attempt_identifier) REFERENCES upload_attempts(identifier) ON DELETE CASCADE,
  UNIQUE (upload_attempt_identifier, failure_code, failed_at)
);

CREATE TABLE IF NOT EXISTS image_usages (
  image_identifier TEXT NOT NULL,
  source_kind TEXT NOT NULL CHECK (source_kind IN ('article', 'memo', 'series')),
  source_identifier TEXT NOT NULL,
  referenced_at TEXT NOT NULL,
  PRIMARY KEY (image_identifier, source_kind, source_identifier),
  FOREIGN KEY (image_identifier) REFERENCES images(identifier) ON DELETE RESTRICT
);

CREATE TABLE IF NOT EXISTS projection_positions (
  projection_name TEXT NOT NULL,
  stream_name TEXT NOT NULL,
  position TEXT NOT NULL,
  applied_at TEXT NOT NULL,
  PRIMARY KEY (projection_name, stream_name)
);

CREATE TABLE IF NOT EXISTS outbox (
  identifier TEXT PRIMARY KEY NOT NULL,
  aggregate_identifier TEXT NOT NULL,
  event_type TEXT NOT NULL,
  payload_json TEXT NOT NULL CHECK (json_valid(payload_json)),
  idempotency_key TEXT NOT NULL UNIQUE,
  occurred_at TEXT NOT NULL,
  published_at TEXT,
  publish_attempts INTEGER NOT NULL DEFAULT 0 CHECK (publish_attempts >= 0),
  last_error TEXT
);

CREATE TABLE IF NOT EXISTS retention_claims (
  image_identifier TEXT PRIMARY KEY NOT NULL,
  claimant_identifier TEXT NOT NULL,
  claimed_at TEXT NOT NULL,
  expires_at TEXT NOT NULL,
  FOREIGN KEY (image_identifier) REFERENCES images(identifier) ON DELETE CASCADE,
  CHECK (expires_at > claimed_at)
);

CREATE INDEX IF NOT EXISTS upload_attempts_by_image_requested_at
  ON upload_attempts (image_identifier, requested_at DESC);
CREATE INDEX IF NOT EXISTS images_by_state_updated_at
  ON images (state, updated_at);
CREATE INDEX IF NOT EXISTS image_inspections_by_image
  ON image_inspections (image_identifier, inspected_at DESC);
CREATE INDEX IF NOT EXISTS inspection_failures_by_attempt
  ON inspection_failures (upload_attempt_identifier, failed_at DESC);
CREATE INDEX IF NOT EXISTS image_usages_by_source
  ON image_usages (source_kind, source_identifier);
CREATE INDEX IF NOT EXISTS outbox_unpublished
  ON outbox (occurred_at) WHERE published_at IS NULL;
CREATE INDEX IF NOT EXISTS retention_claims_by_expiry
  ON retention_claims (expires_at);
