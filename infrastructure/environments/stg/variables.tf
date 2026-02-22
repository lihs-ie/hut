variable "project_id" {
  description = "GCP project ID for the staging environment"
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

variable "reader_container_image" {
  description = "Container image URL for the reader Cloud Run service"
  type        = string
}

variable "admin_container_image" {
  description = "Container image URL for the admin Cloud Run service"
  type        = string
}

variable "worker_container_image" {
  description = "Container image URL for the general worker Cloud Run service"
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

variable "billing_export_service_account" {
  description = "Service account email used by GCP Billing to write export data to BigQuery"
  type        = string
}

variable "billing_export_location" {
  description = "Geographic location for the billing export BigQuery dataset"
  type        = string
}
