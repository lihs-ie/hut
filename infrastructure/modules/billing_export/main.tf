resource "google_bigquery_dataset" "this" {
  dataset_id    = var.dataset_id
  friendly_name = var.friendly_name
  description   = var.description
  project       = var.project_id
  location      = var.location

  default_table_expiration_ms     = var.default_table_expiration_ms
  default_partition_expiration_ms = var.default_partition_expiration_ms

  labels = var.labels
}

resource "google_bigquery_dataset_iam_member" "billing_export_writer" {
  project    = var.project_id
  dataset_id = google_bigquery_dataset.this.dataset_id
  role       = "roles/bigquery.dataEditor"
  member     = "serviceAccount:${var.billing_account_service_account}"
}
