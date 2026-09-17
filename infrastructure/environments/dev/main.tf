locals {
  environment = "dev"
}

module "media_cloudflare" {
  source = "../../modules/media_cloudflare"

  cloudflare_account_id = var.cloudflare_account_id
  cloudflare_zone_id    = var.cloudflare_zone_id
  environment           = local.environment

  asset_custom_domain = "assets.hut.dev.lihs-dev.com"
  enable_r2_dev       = false
  manage_zone_cache   = true
  manage_tiered_cache = false
  r2_location         = var.r2_location

  tmp_upload_cors_allowed_origins = [
    "https://admin.hut.dev.lihs-dev.com",
  ]

  preserved_zone_cache_rules = [{
    ref                 = "562a3fd35dd0487f83cf6cf367ea5249"
    description         = "image cache"
    expression          = "(starts_with(http.request.full_uri, \"https://image.lihs-dev.com/\"))"
    edge_ttl_default    = 31536000
    browser_ttl_default = 86400
    status_code_ttl = [{
      status_code = 200
      value       = 86400
    }]
  }]
}
