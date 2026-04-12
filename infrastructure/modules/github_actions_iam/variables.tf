variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "pool_id" {
  description = "Workload Identity Pool ID"
  type        = string
  default     = "github-actions"
}

variable "provider_id" {
  description = "Workload Identity Pool Provider ID"
  type        = string
  default     = "github-oidc"
}

variable "github_repository" {
  description = "GitHub repository in the format 'owner/repo' for attribute condition"
  type        = string

  validation {
    condition     = can(regex("^[a-zA-Z0-9_.-]+/[a-zA-Z0-9_.-]+$", var.github_repository))
    error_message = "The github_repository must be in the format 'owner/repo'."
  }
}

variable "service_account_id" {
  description = "Service account ID for the GitHub Actions deployer"
  type        = string
  default     = "github-deployer"
}

variable "deployer_roles" {
  description = "IAM roles to grant to the deployer service account. Includes roles/viewer so the SA can run terraform plan against the state bucket and read existing resources in CI."
  type        = list(string)
  default = [
    "roles/run.developer",
    "roles/artifactregistry.writer",
    "roles/iam.serviceAccountUser",
    "roles/datastore.user",
    "roles/viewer",
  ]
}
