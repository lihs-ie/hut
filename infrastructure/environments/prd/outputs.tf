output "admin_service_uri" {
  description = "The URI of the PRD admin Cloud Run service"
  value       = module.cloudrun_admin.service_uri
}

output "image_cleanup_worker_service_uri" {
  description = "The URI of the PRD image cleanup worker Cloud Run service"
  value       = module.cloudrun_image_cleanup_worker.service_uri
}

output "search_token_worker_service_uri" {
  description = "The URI of the PRD search token worker Cloud Run service"
  value       = module.cloudrun_search_token_worker.service_uri
}

output "artifact_registry_url" {
  description = "The URL of the Artifact Registry repository"
  value       = module.artifact_registry.repository_url
}

output "billing_export_dataset_id" {
  description = "The ID of the BigQuery dataset for billing export"
  value       = module.billing_export.dataset_id
}
