variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "GCP region for the Cloud Run service"
  type        = string
}

variable "service_name" {
  description = "Name of the Cloud Run service"
  type        = string
}

variable "container_image" {
  description = "Container image URL for the Cloud Run service"
  type        = string
}

variable "service_account_email" {
  description = "Service account email for the Cloud Run service"
  type        = string
}

variable "min_instance_count" {
  description = "Minimum number of instances"
  type        = number
  default     = 0
}

variable "max_instance_count" {
  description = "Maximum number of instances"
  type        = number
  default     = 2
}

variable "cpu_limit" {
  description = "CPU limit for the container"
  type        = string
  default     = "1"
}

variable "memory_limit" {
  description = "Memory limit for the container"
  type        = string
  default     = "512Mi"
}

variable "cpu_idle" {
  description = "Whether CPU should be throttled when no requests are being served"
  type        = bool
  default     = true
}

variable "startup_cpu_boost" {
  description = "Whether to enable startup CPU boost"
  type        = bool
  default     = true
}

variable "container_port" {
  description = "Container port to expose. Set to null to use Cloud Run default"
  type        = number
  default     = null
}

variable "environment_variables" {
  description = "Map of environment variables to set on the Cloud Run service"
  type        = map(string)
  default     = {}
}

variable "secret_environment_variables" {
  description = "List of secret environment variables to inject from Secret Manager"
  type = list(object({
    name      = string
    secret_id = string
    version   = string
  }))
  default = []
}

variable "allow_unauthenticated" {
  description = "Whether to allow unauthenticated access to the service"
  type        = bool
  default     = false
}

variable "labels" {
  description = "Labels to apply to the Cloud Run service"
  type        = map(string)
  default     = {}
}
