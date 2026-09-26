output "tmp_uploads_bucket_name" {
  description = "Private R2 bucket receiving temporary uploads"
  value       = cloudflare_r2_bucket.tmp_uploads.name
}

output "assets_bucket_name" {
  description = "R2 bucket storing immutable, available Media assets"
  value       = cloudflare_r2_bucket.assets.name
}

output "d1_database_id" {
  description = "D1 database identifier used by Media Workers and Wrangler migrations"
  value       = cloudflare_d1_database.media.id
}

output "d1_database_name" {
  description = "D1 database name used by Media Workers"
  value       = cloudflare_d1_database.media.name
}

output "queue_ids" {
  description = "Media Queue identifiers keyed by responsibility"
  value       = { for key, queue in cloudflare_queue.media : key => queue.queue_id }
}

output "queue_names" {
  description = "Media Queue names keyed by responsibility"
  value       = { for key, queue in cloudflare_queue.media : key => queue.queue_name }
}

output "asset_base_url" {
  description = "Public base URL for immutable Media assets"
  value       = local.custom_domain_enabled ? "https://${var.asset_custom_domain}" : null
}

output "cache_purge_zone_id" {
  description = "Zone ID the retention Worker must target when purging a deleted asset URL"
  value       = local.custom_domain_enabled ? var.cloudflare_zone_id : null
}
