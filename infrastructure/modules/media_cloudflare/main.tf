locals {
  resource_prefix       = "hut-media"
  custom_domain_enabled = var.asset_custom_domain != null && var.cloudflare_zone_id != null
  zone_cache_enabled    = local.custom_domain_enabled && var.manage_zone_cache

  queue_names = {
    inspection             = "${local.resource_prefix}-inspection-${var.environment}"
    inspection_dead_letter = "${local.resource_prefix}-inspection-dlq-${var.environment}"
    reference_projection   = "${local.resource_prefix}-reference-projection-${var.environment}"
    retention              = "${local.resource_prefix}-retention-${var.environment}"
  }
}

resource "cloudflare_r2_bucket" "tmp_uploads" {
  account_id    = var.cloudflare_account_id
  name          = "${local.resource_prefix}-tmp-uploads-${var.environment}"
  location      = var.r2_location
  storage_class = "Standard"

  lifecycle {
    precondition {
      condition     = (var.asset_custom_domain == null) == (var.cloudflare_zone_id == null)
      error_message = "asset_custom_domain and cloudflare_zone_id must be set together."
    }
  }
}

resource "cloudflare_r2_bucket" "assets" {
  account_id    = var.cloudflare_account_id
  name          = "${local.resource_prefix}-assets-${var.environment}"
  location      = var.r2_location
  storage_class = "Standard"
}

resource "cloudflare_r2_bucket_cors" "tmp_uploads" {
  count = length(var.tmp_upload_cors_allowed_origins) > 0 ? 1 : 0

  account_id  = var.cloudflare_account_id
  bucket_name = cloudflare_r2_bucket.tmp_uploads.name
  rules = [{
    id = "browser-direct-upload"
    allowed = {
      origins = var.tmp_upload_cors_allowed_origins
      methods = ["PUT", "HEAD"]
      headers = ["Content-Type"]
    }
    expose_headers  = ["ETag"]
    max_age_seconds = 3600
  }]
}

resource "cloudflare_d1_database" "media" {
  account_id            = var.cloudflare_account_id
  name                  = "${local.resource_prefix}-${var.environment}"
  primary_location_hint = var.r2_location

  lifecycle {
    prevent_destroy = true
  }
}

resource "cloudflare_queue" "media" {
  for_each = local.queue_names

  account_id = var.cloudflare_account_id
  queue_name = each.value
}

resource "cloudflare_r2_bucket_event_notification" "tmp_object_created" {
  account_id  = var.cloudflare_account_id
  bucket_name = cloudflare_r2_bucket.tmp_uploads.name
  queue_id    = cloudflare_queue.media["inspection"].queue_id

  rules = [{
    actions     = ["PutObject", "CopyObject", "CompleteMultipartUpload"]
    description = "Send completed temporary uploads to Media inspection"
  }]
}

# Temporary uploads are never exposed through a Cloudflare-managed public URL.
resource "cloudflare_r2_managed_domain" "tmp_uploads" {
  account_id  = var.cloudflare_account_id
  bucket_name = cloudflare_r2_bucket.tmp_uploads.name
  enabled     = false
}

resource "cloudflare_r2_managed_domain" "assets" {
  account_id  = var.cloudflare_account_id
  bucket_name = cloudflare_r2_bucket.assets.name
  enabled     = var.enable_r2_dev
}

resource "cloudflare_r2_custom_domain" "assets" {
  count = local.custom_domain_enabled ? 1 : 0

  account_id  = var.cloudflare_account_id
  bucket_name = cloudflare_r2_bucket.assets.name
  domain      = var.asset_custom_domain
  enabled     = true
  zone_id     = var.cloudflare_zone_id
  min_tls     = "1.2"
}

resource "cloudflare_ruleset" "asset_cache" {
  count = local.zone_cache_enabled ? 1 : 0

  zone_id     = var.cloudflare_zone_id
  name        = "Hut Media immutable asset cache"
  description = "Cache immutable Media assets at the edge and in browsers"
  kind        = "zone"
  phase       = "http_request_cache_settings"

  rules = [{
    ref         = "hut_media_immutable_assets"
    description = "Cache assets served from ${var.asset_custom_domain}"
    expression  = "(http.host eq \"${var.asset_custom_domain}\")"
    action      = "set_cache_settings"
    enabled     = true
    action_parameters = {
      cache = true
      edge_ttl = {
        mode    = "override_origin"
        default = var.asset_cache_ttl_seconds
      }
      browser_ttl = {
        mode    = "override_origin"
        default = var.asset_cache_ttl_seconds
      }
      cache_key = {
        cache_deception_armor      = true
        ignore_query_strings_order = true
      }
    }
  }]
}

resource "cloudflare_tiered_cache" "assets" {
  count = local.zone_cache_enabled && var.manage_tiered_cache ? 1 : 0

  zone_id = var.cloudflare_zone_id
  value   = "on"
}
