output "reader_service_uri" {
  description = "The URI of the STG reader Cloud Run service"
  value       = module.cloudrun_reader.service_uri
}

output "admin_service_uri" {
  description = "The URI of the STG admin Cloud Run service"
  value       = module.cloudrun_admin.service_uri
}

output "search_token_worker_service_uri" {
  description = "The URI of the STG search token worker Cloud Run service"
  value       = module.cloudrun_search_token_worker.service_uri
}

output "artifact_registry_url" {
  description = "The URL of the Artifact Registry repository"
  value       = module.artifact_registry.repository_url
}

output "workload_identity_provider" {
  description = "Full resource name of the Workload Identity Pool Provider for GitHub Actions"
  value       = module.github_actions_iam.workload_identity_provider
}

output "deployer_service_account_email" {
  description = "Email of the GitHub Actions deployer service account"
  value       = module.github_actions_iam.service_account_email
}

output "billing_export_dataset_id" {
  description = "The ID of the BigQuery dataset for billing export"
  value       = module.billing_export.dataset_id
}
