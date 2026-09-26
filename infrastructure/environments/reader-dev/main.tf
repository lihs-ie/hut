resource "cloudflare_r2_bucket" "incremental_cache" {
  account_id    = var.cloudflare_account_id
  name          = "hut-dev-reader-cache"
  location      = "apac"
  storage_class = "Standard"
}
