# iam

Terraform module for managing Google Cloud IAM service accounts and role bindings.

## Description

Creates service accounts and assigns IAM roles at the project level using `google_project_iam_member`. Supports multiple service accounts with multiple roles each, using a flat binding approach.

## Usage

```hcl
module "iam" {
  source = "../../modules/iam"

  project_id = "my-project"

  service_accounts = {
    "my-app-sa" = {
      display_name = "My Application Service Account"
      description  = "Service account for the application"
      roles = [
        "roles/datastore.user",
        "roles/pubsub.publisher",
      ]
    }
    "my-worker-sa" = {
      display_name = "My Worker Service Account"
      description  = "Service account for the worker"
      roles = [
        "roles/datastore.user",
        "roles/eventarc.eventReceiver",
      ]
    }
  }
}
```

## Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|----------|
| project_id | GCP project ID | `string` | - | yes |
| service_accounts | Map of service account configurations. Key is the account ID | `map(object)` | - | yes |

### service_accounts object

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|----------|
| display_name | Display name for the service account | `string` | - | yes |
| description | Description for the service account | `string` | `""` | no |
| roles | List of IAM roles to assign | `list(string)` | `[]` | no |

## Outputs

| Name | Description |
|------|-------------|
| service_account_emails | Map of service account ID to email |
| service_account_names | Map of service account ID to fully qualified name |

## Design Decisions

This module uses `google_project_iam_member` (additive bindings) rather than `google_project_iam_binding` or `google_project_iam_policy`. This is the recommended approach for Terraform-managed IAM because it does not remove bindings managed outside of Terraform.

IAM bindings are scoped at the project level. This is acceptable because each environment uses a dedicated GCP project (e.g. `hut-stg`, `hut-prd`), providing isolation. If multiple applications share a single project in the future, consider migrating to resource-level IAM bindings for finer-grained access control.

## Requirements

| Name | Version |
|------|---------|
| terraform | ~> 1.9 |
| google | ~> 6.0 |

## Attribution

This module was created following best practices from [terraform-skill](https://github.com/antonbabenko/terraform-skill) by Anton Babenko.
