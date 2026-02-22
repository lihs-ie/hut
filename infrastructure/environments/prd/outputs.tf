output "admin_service_uri" {
  description = "The URI of the PRD admin Cloud Run service"
  value       = module.cloudrun_admin.service_uri
}

output "image_cleanup_worker_service_uri" {
  description = "The URI of the PRD image cleanup worker Cloud Run service"
  value       = module.cloudrun_image_cleanup_worker.service_uri
}

output "worker_service_uri" {
  description = "The URI of the PRD general worker Cloud Run service"
  value       = module.cloudrun_worker.service_uri
}

output "artifact_registry_url" {
  description = "The URL of the Artifact Registry repository"
  value       = module.artifact_registry.repository_url
}
