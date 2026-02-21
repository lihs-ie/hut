# firestore

Terraform module for managing Google Firestore databases.

## Description

Creates a Firestore database in Native mode with configurable delete protection and optional daily automatic backup scheduling.

## Usage

```hcl
module "firestore" {
  source = "../../modules/firestore"

  project_id          = "my-project"
  location_id         = "asia-northeast1"
  database_name       = "(default)"
  delete_protection   = true
  enable_daily_backup = true
  backup_retention    = "604800s"
}
```

## Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|----------|
| project_id | GCP project ID | `string` | - | yes |
| database_name | Name of the Firestore database | `string` | `"(default)"` | no |
| location_id | Location for the Firestore database | `string` | - | yes |
| delete_protection | Whether to enable delete protection on the database | `bool` | `false` | no |
| enable_daily_backup | Whether to enable daily automatic backup of the Firestore database | `bool` | `false` | no |
| backup_retention | Retention period for Firestore backups in duration format (e.g. 604800s for 7 days) | `string` | `"604800s"` | no |

## Outputs

| Name | Description |
|------|-------------|
| database_name | The name of the Firestore database |
| database_id | The ID of the Firestore database |

## Notes

- The `google_firestore_database` resource does not support the `labels` attribute. This is a limitation of the GCP provider.
- Daily backup uses `google_firestore_backup_schedule` which is available in Google Provider v6+.
- Backup retention of 604800s (7 days) is the default. Adjust based on your recovery requirements.

## Requirements

| Name | Version |
|------|---------|
| terraform | ~> 1.9 |
| google | ~> 6.0 |

## Attribution

This module was created following best practices from [terraform-skill](https://github.com/antonbabenko/terraform-skill) by Anton Babenko.
