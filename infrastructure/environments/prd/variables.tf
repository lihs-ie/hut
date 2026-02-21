variable "project_id" {
  description = "GCP project ID for the production environment"
  type        = string
}

variable "region" {
  description = "GCP region for all resources"
  type        = string
}

variable "firestore_location" {
  description = "Location for the Firestore database"
  type        = string
}

variable "cloudflare_api_token" {
  description = "Cloudflare API token for managing Workers"
  type        = string
  sensitive   = true
}

variable "cloudflare_account_id" {
  description = "Cloudflare account ID"
  type        = string
}

variable "admin_container_image" {
  description = "Container image URL for the admin Cloud Run service"
  type        = string
}

variable "image_cleanup_worker_container_image" {
  description = "Container image URL for the image cleanup worker Cloud Run service"
  type        = string
}

variable "worker_container_image" {
  description = "Container image URL for the general worker Cloud Run service"
  type        = string
}
