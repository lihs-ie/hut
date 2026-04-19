output "incremental_cache_bucket_name" {
  description = "R2 bucket name used for OpenNext incremental cache"
  value       = cloudflare_r2_bucket.incremental_cache.name
}
