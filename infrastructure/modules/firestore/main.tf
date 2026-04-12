resource "google_firestore_database" "this" {
  name        = var.database_name
  project     = var.project_id
  location_id = var.location_id
  type        = "FIRESTORE_NATIVE"

  delete_protection_state = var.delete_protection ? "DELETE_PROTECTION_ENABLED" : "DELETE_PROTECTION_DISABLED"
}

resource "google_firebaserules_ruleset" "firestore" {
  count   = var.rules_file != null ? 1 : 0
  project = var.project_id

  source {
    files {
      name    = "firestore.rules"
      content = file(var.rules_file)
    }
  }
}

resource "google_firebaserules_release" "firestore" {
  count        = var.rules_file != null ? 1 : 0
  project      = var.project_id
  name         = "cloud.firestore/database/${google_firestore_database.this.name}"
  ruleset_name = google_firebaserules_ruleset.firestore[0].name
}

resource "google_firestore_backup_schedule" "daily" {
  count = var.enable_daily_backup ? 1 : 0

  project  = var.project_id
  database = google_firestore_database.this.name

  retention = var.backup_retention

  daily_recurrence {}
}

resource "google_firestore_index" "composite" {
  for_each = { for index in var.composite_indexes : index.name => index }

  project     = var.project_id
  database    = google_firestore_database.this.name
  collection  = each.value.collection
  query_scope = each.value.query_scope

  dynamic "fields" {
    for_each = each.value.fields
    content {
      field_path = fields.value.field_path
      order      = fields.value.order
    }
  }
}
