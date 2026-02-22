variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "database_name" {
  description = "Name of the Firestore database"
  type        = string
  default     = "(default)"
}

variable "location_id" {
  description = "Location for the Firestore database"
  type        = string
}

variable "delete_protection" {
  description = "Whether to enable delete protection on the database"
  type        = bool
  default     = false
}

variable "enable_daily_backup" {
  description = "Whether to enable daily automatic backup of the Firestore database"
  type        = bool
  default     = false
}

variable "backup_retention" {
  description = "Retention period for Firestore backups in duration format (e.g. 604800s for 7 days)"
  type        = string
  default     = "604800s"
}
