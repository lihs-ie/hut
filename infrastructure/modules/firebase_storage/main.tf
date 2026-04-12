resource "google_storage_bucket" "this" {
  name     = var.bucket_name
  location = var.location
  project  = var.project_id

  uniform_bucket_level_access = var.uniform_bucket_level_access

  versioning {
    enabled = var.versioning_enabled
  }

  dynamic "cors" {
    for_each = length(var.cors_origins) > 0 ? [1] : []

    content {
      origin          = var.cors_origins
      method          = var.cors_methods
      response_header = var.cors_response_headers
      max_age_seconds = var.cors_max_age_seconds
    }
  }

  labels = var.labels
}

resource "google_firebaserules_ruleset" "storage" {
  count   = var.rules_file != null ? 1 : 0
  project = var.project_id

  source {
    files {
      name    = "storage.rules"
      content = file(var.rules_file)
    }
  }
}

resource "google_firebaserules_release" "storage" {
  count        = var.rules_file != null ? 1 : 0
  project      = var.project_id
  name         = "firebase.storage/${google_storage_bucket.this.name}"
  ruleset_name = google_firebaserules_ruleset.storage[0].name
}
