variable "cloudflare_account_id" {
  description = "Cloudflare account ID that owns the Media dev resources"
  type        = string

  validation {
    condition     = trimspace(var.cloudflare_account_id) != ""
    error_message = "cloudflare_account_id must not be empty."
  }
}

variable "cloudflare_zone_id" {
  description = "Cloudflare zone ID for lihs-dev.com"
  type        = string

  validation {
    condition     = trimspace(var.cloudflare_zone_id) != ""
    error_message = "cloudflare_zone_id must not be empty."
  }
}

variable "r2_location" {
  description = "Location hint for Media dev R2 buckets and D1"
  type        = string
  default     = "apac"

  validation {
    condition     = contains(["apac", "eeur", "enam", "weur", "wnam", "oc"], var.r2_location)
    error_message = "r2_location must be a location supported by Cloudflare R2."
  }
}
