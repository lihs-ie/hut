output "incremental_cache_bucket_name" {
  description = "R2 bucket for the dev Reader incremental cache"
  value       = cloudflare_r2_bucket.incremental_cache.name
}
