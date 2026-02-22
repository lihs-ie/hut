resource "google_firestore_database" "this" {
  name        = var.database_name
  project     = var.project_id
  location_id = var.location_id
  type        = "FIRESTORE_NATIVE"

  delete_protection_state = var.delete_protection ? "DELETE_PROTECTION_ENABLED" : "DELETE_PROTECTION_DISABLED"
}

resource "google_firestore_backup_schedule" "daily" {
  count = var.enable_daily_backup ? 1 : 0

  project  = var.project_id
  database = google_firestore_database.this.name

  retention = var.backup_retention

  daily_recurrence {}
}
