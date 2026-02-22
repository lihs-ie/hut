resource "google_pubsub_topic" "this" {
  name    = var.topic_name
  project = var.project_id

  message_retention_duration = var.message_retention_duration

  labels = var.labels
}

resource "google_pubsub_subscription" "this" {
  for_each = var.subscriptions

  name    = each.key
  project = var.project_id
  topic   = google_pubsub_topic.this.id

  ack_deadline_seconds       = each.value.ack_deadline_seconds
  message_retention_duration = each.value.message_retention_duration

  retry_policy {
    minimum_backoff = each.value.minimum_backoff
    maximum_backoff = each.value.maximum_backoff
  }

  labels = var.labels
}
