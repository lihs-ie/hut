variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "service_accounts" {
  description = "Map of service account configurations. Key is the account ID"
  type = map(object({
    display_name = string
    description  = optional(string, "")
    roles        = optional(list(string), [])
  }))
}
