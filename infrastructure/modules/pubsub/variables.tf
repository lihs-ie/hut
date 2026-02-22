variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "topic_name" {
  description = "Name of the Pub/Sub topic"
  type        = string
}

variable "message_retention_duration" {
  description = "Message retention duration for the topic"
  type        = string
  default     = "86400s"
}

variable "subscriptions" {
  description = "Map of subscription configurations"
  type = map(object({
    ack_deadline_seconds       = optional(number, 20)
    message_retention_duration = optional(string, "604800s")
    minimum_backoff            = optional(string, "10s")
    maximum_backoff            = optional(string, "600s")
  }))
  default = {}
}

variable "labels" {
  description = "Labels to apply to Pub/Sub resources"
  type        = map(string)
  default     = {}
}
