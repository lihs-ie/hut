variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "secrets" {
  description = "Map of secret configurations. Key is the secret ID, value contains accessor members"
  type = map(object({
    accessor_members = optional(list(string), [])
  }))
}

variable "labels" {
  description = "Labels to apply to secrets"
  type        = map(string)
  default     = {}
}
