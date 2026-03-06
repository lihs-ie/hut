variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "authorized_domains" {
  description = "List of domains authorized for OAuth redirects"
  type        = list(string)
}

variable "oauth_client_id" {
  description = "OAuth client ID for Google sign-in"
  type        = string
  sensitive   = true
}

variable "oauth_client_secret" {
  description = "OAuth client secret for Google sign-in"
  type        = string
  sensitive   = true
}
