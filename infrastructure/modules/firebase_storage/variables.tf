variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "bucket_name" {
  description = "Name of the Cloud Storage bucket"
  type        = string
}

variable "location" {
  description = "Location for the storage bucket"
  type        = string
}

variable "uniform_bucket_level_access" {
  description = "Whether to enable uniform bucket-level access"
  type        = bool
  default     = true
}

variable "versioning_enabled" {
  description = "Whether to enable object versioning"
  type        = bool
  default     = false
}

variable "cors_origins" {
  description = "List of allowed CORS origins"
  type        = list(string)
  default     = []
}

variable "cors_methods" {
  description = "List of allowed CORS methods"
  type        = list(string)
  default     = ["GET", "HEAD", "OPTIONS"]
}

variable "cors_response_headers" {
  description = "List of allowed CORS response headers"
  type        = list(string)
  default     = ["Content-Type"]
}

variable "cors_max_age_seconds" {
  description = "Maximum age in seconds for CORS preflight cache"
  type        = number
  default     = 3600
}

variable "labels" {
  description = "Labels to apply to the storage bucket"
  type        = map(string)
  default     = {}
}
