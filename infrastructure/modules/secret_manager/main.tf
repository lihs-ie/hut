locals {
  secret_accessor_pairs = flatten([
    for secret_key, secret_config in var.secrets : [
      for member in secret_config.accessor_members : {
        secret_id = secret_key
        member    = member
      }
    ]
  ])
}

resource "google_secret_manager_secret" "this" {
  for_each = var.secrets

  secret_id = each.key
  project   = var.project_id

  replication {
    auto {}
  }

  labels = var.labels
}

resource "google_secret_manager_secret_iam_member" "accessor" {
  for_each = { for pair in local.secret_accessor_pairs : "${pair.secret_id}-${pair.member}" => pair }

  project   = var.project_id
  secret_id = google_secret_manager_secret.this[each.value.secret_id].secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value.member
}
