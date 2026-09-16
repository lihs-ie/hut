output "media_cloudflare" {
  description = "Cloudflare Media dev resource identifiers used by Worker deployment"
  value = {
    tmp_uploads_bucket_name = module.media_cloudflare.tmp_uploads_bucket_name
    assets_bucket_name      = module.media_cloudflare.assets_bucket_name
    asset_base_url          = module.media_cloudflare.asset_base_url
    cache_purge_zone_id     = module.media_cloudflare.cache_purge_zone_id
    d1_database_id          = module.media_cloudflare.d1_database_id
    d1_database_name        = module.media_cloudflare.d1_database_name
    queue_ids               = module.media_cloudflare.queue_ids
    queue_names             = module.media_cloudflare.queue_names
  }
}
