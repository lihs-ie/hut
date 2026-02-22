# artifact_registry

Terraform module for managing Google Artifact Registry repositories.

## Description

Creates a Docker-format Artifact Registry repository with configurable cleanup policies to manage image retention.

## Usage

```hcl
module "artifact_registry" {
  source = "../../modules/artifact_registry"

  project_id    = "my-project"
  region        = "asia-northeast1"
  repository_id = "my-repo"
  description   = "Docker images for my application"
  keep_count    = 10

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
| region | GCP region for the Artifact Registry repository | `string` | - | yes |
| repository_id | ID of the Artifact Registry repository | `string` | - | yes |
| description | Description of the repository | `string` | `""` | no |
| keep_count | Number of recent image versions to keep | `number` | `10` | no |
| labels | Labels to apply to the repository | `map(string)` | `{}` | no |

## Outputs

| Name | Description |
|------|-------------|
| repository_id | The ID of the Artifact Registry repository |
| repository_name | The full resource name of the repository |
| repository_url | The URL of the repository for pushing images |

## Requirements

| Name | Version |
|------|---------|
| terraform | ~> 1.9 |
| google | ~> 6.0 |

## Attribution

This module was created following best practices from [terraform-skill](https://github.com/antonbabenko/terraform-skill) by Anton Babenko.
