variable "cloudflare_account_id" {
  description = "Cloudflare account ID hosting the reader Worker"
  type        = string
}

variable "incremental_cache_bucket_name" {
  description = "R2 bucket name used by OpenNext for Next.js incremental cache (ISR)"
  type        = string
  default     = "hut-prd-reader-cache"
}

variable "incremental_cache_bucket_location" {
  description = "R2 bucket location hint (apac, eeur, enam, weur, wnam, oc)"
  type        = string
  default     = "apac"
}
