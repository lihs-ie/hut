# cloudrun_service

Terraform module for managing Google Cloud Run v2 services.

## Description

Creates a Cloud Run v2 service with configurable container settings, environment variables, secret injection from Secret Manager, and optional public access.

## Usage

```hcl
module "cloudrun_service" {
  source = "../../modules/cloudrun_service"

  project_id            = "my-project"
  region                = "asia-northeast1"
  service_name          = "my-service"
  container_image       = "asia-northeast1-docker.pkg.dev/my-project/my-repo/my-image:latest"
  service_account_email = "my-sa@my-project.iam.gserviceaccount.com"
  allow_unauthenticated = true

  environment_variables = {
    FIREBASE_PROJECT_ID = "my-project"
  }

  secret_environment_variables = [
    {
      name      = "API_KEY"
      secret_id = "my-api-key"
      version   = "latest"
    },
  ]

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
| region | GCP region for the Cloud Run service | `string` | - | yes |
| service_name | Name of the Cloud Run service | `string` | - | yes |
| container_image | Container image URL for the Cloud Run service | `string` | - | yes |
| service_account_email | Service account email for the Cloud Run service | `string` | - | yes |
| min_instance_count | Minimum number of instances | `number` | `0` | no |
| max_instance_count | Maximum number of instances | `number` | `2` | no |
| cpu_limit | CPU limit for the container | `string` | `"1"` | no |
| memory_limit | Memory limit for the container | `string` | `"512Mi"` | no |
| cpu_idle | Whether CPU should be throttled when no requests are being served | `bool` | `true` | no |
| startup_cpu_boost | Whether to enable startup CPU boost | `bool` | `true` | no |
| container_port | Container port to expose. Set to null to use Cloud Run default | `number` | `null` | no |
| environment_variables | Map of environment variables to set on the Cloud Run service | `map(string)` | `{}` | no |
| secret_environment_variables | List of secret environment variables to inject from Secret Manager | `list(object)` | `[]` | no |
| allow_unauthenticated | Whether to allow unauthenticated access to the service | `bool` | `false` | no |
| labels | Labels to apply to the Cloud Run service | `map(string)` | `{}` | no |

## Outputs

| Name | Description |
|------|-------------|
| service_id | The ID of the Cloud Run service |
| service_name | The name of the Cloud Run service |
| service_uri | The URI of the Cloud Run service |
| service_location | The location of the Cloud Run service |

## Notes

- The `container_image` is set to `ignore_changes` in the lifecycle block to allow CI/CD pipelines to update the image without Terraform drift.
- Scale-to-zero is enabled by default (`min_instance_count = 0`, `cpu_idle = true`).

## Requirements

| Name | Version |
|------|---------|
| terraform | ~> 1.9 |
| google | ~> 6.0 |

## Attribution

This module was created following best practices from [terraform-skill](https://github.com/antonbabenko/terraform-skill) by Anton Babenko.
