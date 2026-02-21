# secret_manager

Terraform module for managing Google Secret Manager secrets and access control.

## Description

Creates Secret Manager secrets with automatic replication and configurable IAM access bindings. Secret values are managed outside of Terraform (via GCP Console or gcloud CLI) to avoid storing sensitive data in Terraform state.

## Usage

```hcl
module "secrets" {
  source = "../../modules/secret_manager"

  project_id = "my-project"

  secrets = {
    "my-api-key" = {
      accessor_members = [
        "serviceAccount:my-sa@my-project.iam.gserviceaccount.com",
      ]
    }
    "my-database-password" = {
      accessor_members = [
        "serviceAccount:my-app@my-project.iam.gserviceaccount.com",
        "serviceAccount:my-worker@my-project.iam.gserviceaccount.com",
      ]
    }
  }

  labels = {
    environment = "production"
    managed_by  = "terraform"
  }
}
```

## Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|----------|
| project_id | GCP project ID | `string` | - | yes |
| secrets | Map of secret configurations. Key is the secret ID, value contains accessor members | `map(object)` | - | yes |
| labels | Labels to apply to secrets | `map(string)` | `{}` | no |

### secrets object

| Name | Description | Type | Default |
|------|-------------|------|---------|
| accessor_members | List of IAM members to grant secretAccessor role | `list(string)` | `[]` |

## Outputs

| Name | Description |
|------|-------------|
| secret_ids | Map of secret name to secret ID |
| secret_names | Map of secret name to fully qualified secret name |

## Notes

- This module creates secret containers only. Secret values must be populated separately via GCP Console, gcloud CLI, or CI/CD pipeline.
- IAM bindings use `google_secret_manager_secret_iam_member` (additive) to avoid conflicts with bindings managed outside Terraform.

## Requirements

| Name | Version |
|------|---------|
| terraform | ~> 1.9 |
| google | ~> 6.0 |

## Attribution

This module was created following best practices from [terraform-skill](https://github.com/antonbabenko/terraform-skill) by Anton Babenko.
