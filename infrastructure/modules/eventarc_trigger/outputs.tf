output "trigger_id" {
  description = "The ID of the Eventarc trigger"
  value       = google_eventarc_trigger.this.id
}

output "trigger_name" {
  description = "The name of the Eventarc trigger"
  value       = google_eventarc_trigger.this.name
}
