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
