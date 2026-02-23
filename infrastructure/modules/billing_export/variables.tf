variable "project_id" {
  description = "GCP project ID where the BigQuery dataset will be created"
  type        = string
}

variable "dataset_id" {
  description = "BigQuery dataset ID for billing export data"
  type        = string
  default     = "billing_export"

  validation {
    condition     = can(regex("^[a-zA-Z_][a-zA-Z0-9_]+$", var.dataset_id)) && length(var.dataset_id) <= 1024
    error_message = "The dataset_id must start with a letter or underscore, contain only letters, numbers, and underscores, and be at most 1024 characters."
  }
}

variable "friendly_name" {
  description = "Human-readable name for the BigQuery dataset"
  type        = string
  default     = "Billing Export"
}

variable "description" {
  description = "Description of the BigQuery dataset for billing export"
  type        = string
  default     = "GCP billing export data (Standard usage cost)"
}

variable "location" {
  description = "Geographic location where the BigQuery dataset should reside"
  type        = string
}

variable "default_table_expiration_ms" {
  description = "Default expiration time for tables in the dataset in milliseconds (null for no expiration)"
  type        = number
  default     = null
}

variable "default_partition_expiration_ms" {
  description = "Default partition expiration time for partitioned tables in milliseconds (null for no expiration)"
  type        = number
  default     = null
}

variable "billing_account_service_account" {
  description = "Service account email used by GCP Billing to write export data (e.g., billing-export-bigquery@system.gserviceaccount.com)"
  type        = string

  validation {
    condition     = can(regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", var.billing_account_service_account))
    error_message = "The billing_account_service_account must be a valid email address."
  }
}

variable "labels" {
  description = "Labels to apply to the BigQuery dataset"
  type        = map(string)
  default     = {}
}
