resource "cloudflare_r2_bucket" "incremental_cache" {
  account_id = var.cloudflare_account_id
  name       = var.incremental_cache_bucket_name
  location   = var.incremental_cache_bucket_location
}
