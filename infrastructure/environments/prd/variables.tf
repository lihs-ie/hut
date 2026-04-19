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

variable "admin_container_image" {
  description = "Container image URL for the admin Cloud Run service"
  type        = string
}

variable "image_cleanup_worker_container_image" {
  description = "Container image URL for the image cleanup worker Cloud Run service"
  type        = string
}

variable "search_token_worker_container_image" {
  description = "Container image URL for the search token worker Cloud Run service"
  type        = string
}

variable "authorized_members" {
  description = "List of IAM members authorized to invoke Cloud Run services (e.g. user:example@gmail.com)"
  type        = list(string)
}

variable "github_repository" {
  description = "GitHub repository in the format 'owner/repo' for Workload Identity Federation"
  type        = string

  validation {
    condition     = can(regex("^[a-zA-Z0-9_.-]+/[a-zA-Z0-9_.-]+$", var.github_repository))
    error_message = "The github_repository must be in the format 'owner/repo'."
  }
}

variable "oauth_client_id" {
  description = "OAuth client ID for Google sign-in (Identity Platform)"
  type        = string
  sensitive   = true
}

variable "oauth_client_secret" {
  description = "OAuth client secret for Google sign-in (Identity Platform)"
  type        = string
  sensitive   = true
}

variable "billing_export_service_account" {
  description = "Service account email used by GCP Billing to write export data to BigQuery"
  type        = string
}

variable "billing_export_location" {
  description = "Geographic location for the billing export BigQuery dataset"
  type        = string
}

variable "billing_account_id" {
  description = "GCP billing account ID that owns the project and the monthly budget"
  type        = string
}

variable "notification_email" {
  description = "Email address that receives GCP budget alert notifications"
  type        = string
  sensitive   = true
}

# TODO: Cloudflare API token / account ID を GitHub Secrets / tfvars に登録した後、
# 以下を有効化して versions.tf の cloudflare provider と main.tf の module 呼び出しも有効化する。
# variable "cloudflare_account_id" {
#   description = "Cloudflare account ID hosting the reader Cloudflare Worker"
#   type        = string
# }
#
# variable "cloudflare_api_token" {
#   description = "Cloudflare API token with Workers / R2 edit scope"
#   type        = string
#   sensitive   = true
# }
