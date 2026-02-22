resource "google_artifact_registry_repository" "this" {
  repository_id = var.repository_id
  location      = var.region
  project       = var.project_id
  format        = "DOCKER"
  description   = var.description

  cleanup_policies {
    id     = "keep-recent"
    action = "KEEP"

    most_recent_versions {
      keep_count = var.keep_count
    }
  }

  labels = var.labels
}
