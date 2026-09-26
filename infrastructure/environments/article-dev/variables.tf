variable "cloudflare_account_id" {
  description = "Cloudflare account ID that owns the Article dev queues"
  type        = string

  validation {
    condition     = trimspace(var.cloudflare_account_id) != ""
    error_message = "cloudflare_account_id must not be empty."
  }
}
