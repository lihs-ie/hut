variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "GCP region for the Eventarc trigger"
  type        = string
}

variable "trigger_name" {
  description = "Name of the Eventarc trigger"
  type        = string
}

variable "pubsub_topic_id" {
  description = "Full resource ID of the Pub/Sub topic to subscribe to"
  type        = string
}

variable "cloudrun_service_name" {
  description = "Name of the Cloud Run service to route events to"
  type        = string
}

variable "destination_path" {
  description = "HTTP path on the Cloud Run service to receive events"
  type        = string
  default     = "/"
}

variable "service_account_email" {
  description = "Service account email for the Eventarc trigger"
  type        = string
}

variable "labels" {
  description = "Labels to apply to the Eventarc trigger"
  type        = map(string)
  default     = {}
}
