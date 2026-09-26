output "queue_names" {
  description = "Article dev Queue names consumed by the Worker configs"
  value       = local.queue_names
}

output "queue_ids" {
  description = "Article dev Queue identifiers"
  value       = { for key, queue in cloudflare_queue.article : key => queue.queue_id }
}
