locals {
  role_bindings = flatten([
    for account_id, account_config in var.service_accounts : [
      for role in account_config.roles : {
        account_id = account_id
        role       = role
      }
    ]
  ])
}

resource "google_service_account" "this" {
  for_each = var.service_accounts

  account_id   = each.key
  display_name = each.value.display_name
  description  = each.value.description
  project      = var.project_id
}

resource "google_project_iam_member" "this" {
  for_each = { for binding in local.role_bindings : "${binding.account_id}-${binding.role}" => binding }

  project = var.project_id
  role    = each.value.role
  member  = "serviceAccount:${google_service_account.this[each.value.account_id].email}"
}
