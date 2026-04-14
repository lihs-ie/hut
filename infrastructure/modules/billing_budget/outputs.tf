output "budget_name" {
  description = "Fully qualified resource name of the billing budget"
  value       = google_billing_budget.this.name
}

output "notification_channel_id" {
  description = "Monitoring notification channel identifier for the budget alerts"
  value       = google_monitoring_notification_channel.email.id
}
