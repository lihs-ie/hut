resource "google_firestore_database" "this" {
  name        = var.database_name
  project     = var.project_id
  location_id = var.location_id
  type        = "FIRESTORE_NATIVE"

  delete_protection_state = var.delete_protection ? "DELETE_PROTECTION_ENABLED" : "DELETE_PROTECTION_DISABLED"
}
