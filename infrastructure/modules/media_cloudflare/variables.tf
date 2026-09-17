variable "cloudflare_account_id" {
  description = "Cloudflare account ID that owns the Media resources"
  type        = string
}

variable "environment" {
  description = "Deployment environment suffix"
  type        = string

  validation {
    condition     = contains(["dev", "stg", "prd"], var.environment)
    error_message = "environment must be dev, stg, or prd."
  }
}

variable "r2_location" {
  description = "Location hint for Media R2 buckets and D1"
  type        = string
  default     = "apac"

  validation {
    condition     = contains(["apac", "eeur", "enam", "weur", "wnam", "oc"], var.r2_location)
    error_message = "r2_location must be a location supported by Cloudflare R2."
  }
}

variable "asset_custom_domain" {
  description = "Custom domain for immutable Media assets; null disables it"
  type        = string
  default     = null
  nullable    = true
}

variable "cloudflare_zone_id" {
  description = "Cloudflare zone containing asset_custom_domain"
  type        = string
  default     = null
  nullable    = true
}

variable "enable_r2_dev" {
  description = "Whether to expose the asset bucket through r2.dev"
  type        = bool
  default     = false
}

variable "tmp_upload_cors_allowed_origins" {
  description = "Browser origins allowed to upload directly to the private temporary R2 bucket"
  type        = list(string)
  default     = []

  validation {
    condition = alltrue([
      for origin in var.tmp_upload_cors_allowed_origins :
      can(regex("^https://[^/]+$", origin))
    ])
    error_message = "tmp_upload_cors_allowed_origins must contain HTTPS origins without paths."
  }
}

variable "manage_zone_cache" {
  description = "Whether this module manages the Media cache rule"
  type        = bool
  default     = true
}

variable "preserved_zone_cache_rules" {
  description = "Pre-existing rules that must remain in the zone cache ruleset"
  type = list(object({
    ref                 = string
    description         = string
    expression          = string
    edge_ttl_default    = number
    browser_ttl_default = number
    status_code_ttl = list(object({
      status_code = number
      value       = number
    }))
  }))
  default = []
}

variable "manage_tiered_cache" {
  description = "Whether this module owns the zone-wide Smart Tiered Cache setting"
  type        = bool
  default     = true
}

variable "asset_cache_ttl_seconds" {
  description = "Browser and edge cache lifetime for immutable Media assets"
  type        = number
  default     = 31536000

  validation {
    condition     = var.asset_cache_ttl_seconds > 0
    error_message = "asset_cache_ttl_seconds must be greater than zero."
  }
}
