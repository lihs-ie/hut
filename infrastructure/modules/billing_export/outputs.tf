output "dataset_id" {
  description = "The ID of the BigQuery dataset for billing export"
  value       = google_bigquery_dataset.this.dataset_id
}

output "dataset_self_link" {
  description = "The URI of the BigQuery dataset for billing export"
  value       = google_bigquery_dataset.this.self_link
}

output "dataset_project" {
  description = "The project ID where the BigQuery dataset is created"
  value       = google_bigquery_dataset.this.project
}
