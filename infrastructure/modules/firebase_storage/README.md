# firebase_storage

Terraform module for managing Firebase Storage (Google Cloud Storage) buckets.

## Description

Creates a Google Cloud Storage bucket configured for Firebase Storage with optional CORS settings, versioning, and uniform bucket-level access.

## Usage

```hcl
module "firebase_storage" {
  source = "../../modules/firebase_storage"

  project_id         = "my-project"
  bucket_name        = "my-project.firebasestorage.app"
  location           = "asia-northeast1"
  versioning_enabled = true

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
| bucket_name | Name of the Cloud Storage bucket | `string` | - | yes |
| location | Location for the storage bucket | `string` | - | yes |
| uniform_bucket_level_access | Whether to enable uniform bucket-level access | `bool` | `true` | no |
| versioning_enabled | Whether to enable object versioning | `bool` | `false` | no |
| cors_origins | List of allowed CORS origins | `list(string)` | `[]` | no |
| cors_methods | List of allowed CORS methods | `list(string)` | `["GET", "HEAD", "OPTIONS"]` | no |
| cors_response_headers | List of allowed CORS response headers | `list(string)` | `["Content-Type"]` | no |
| cors_max_age_seconds | Maximum age in seconds for CORS preflight cache | `number` | `3600` | no |
| labels | Labels to apply to the storage bucket | `map(string)` | `{}` | no |

## Outputs

| Name | Description |
|------|-------------|
| bucket_name | The name of the storage bucket |
| bucket_url | The URL of the storage bucket |
| bucket_self_link | The self link of the storage bucket |

## Requirements

| Name | Version |
|------|---------|
| terraform | ~> 1.9 |
| google | ~> 6.0 |

## Attribution

This module was created following best practices from [terraform-skill](https://github.com/antonbabenko/terraform-skill) by Anton Babenko.
