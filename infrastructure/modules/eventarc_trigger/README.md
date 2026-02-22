# eventarc_trigger

Terraform module for managing Google Eventarc triggers.

## Description

Creates an Eventarc trigger that routes Pub/Sub messages to a Cloud Run service. Configured for the `google.cloud.pubsub.topic.v1.messagePublished` event type.

## Usage

```hcl
module "eventarc_trigger" {
  source = "../../modules/eventarc_trigger"

  project_id            = "my-project"
  region                = "asia-northeast1"
  trigger_name          = "my-trigger"
  pubsub_topic_id       = module.pubsub.topic_id
  cloudrun_service_name = module.cloudrun.service_name
  service_account_email = "my-sa@my-project.iam.gserviceaccount.com"

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
| region | GCP region for the Eventarc trigger | `string` | - | yes |
| trigger_name | Name of the Eventarc trigger | `string` | - | yes |
| pubsub_topic_id | Full resource ID of the Pub/Sub topic to subscribe to | `string` | - | yes |
| cloudrun_service_name | Name of the Cloud Run service to route events to | `string` | - | yes |
| destination_path | HTTP path on the Cloud Run service to receive events | `string` | `"/"` | no |
| service_account_email | Service account email for the Eventarc trigger | `string` | - | yes |
| labels | Labels to apply to the Eventarc trigger | `map(string)` | `{}` | no |

## Outputs

| Name | Description |
|------|-------------|
| trigger_id | The ID of the Eventarc trigger |
| trigger_name | The name of the Eventarc trigger |

## Requirements

| Name | Version |
|------|---------|
| terraform | ~> 1.9 |
| google | ~> 6.0 |

## Attribution

This module was created following best practices from [terraform-skill](https://github.com/antonbabenko/terraform-skill) by Anton Babenko.
