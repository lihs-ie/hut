output "topic_id" {
  description = "The ID of the Pub/Sub topic"
  value       = google_pubsub_topic.this.id
}

output "topic_name" {
  description = "The name of the Pub/Sub topic"
  value       = google_pubsub_topic.this.name
}

output "subscription_ids" {
  description = "Map of subscription name to subscription ID"
  value       = { for key, subscription in google_pubsub_subscription.this : key => subscription.id }
}
