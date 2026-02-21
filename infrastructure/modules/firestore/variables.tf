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
