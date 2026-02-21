output "service_account_emails" {
  description = "Map of service account ID to email"
  value       = { for key, account in google_service_account.this : key => account.email }
}

output "service_account_names" {
  description = "Map of service account ID to fully qualified name"
  value       = { for key, account in google_service_account.this : key => account.name }
}
