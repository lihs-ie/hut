# pubsub

Terraform module for managing Google Cloud Pub/Sub topics and subscriptions.

## Description

Creates a Pub/Sub topic with optional subscriptions. Each subscription can be configured with custom acknowledgment deadlines, message retention, and retry policies.

## Usage

```hcl
module "pubsub" {
  source = "../../modules/pubsub"

  project_id = "my-project"
  topic_name = "image-events"

  subscriptions = {
    "image-cleanup-sub" = {
      ack_deadline_seconds = 30
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
| topic_name | Name of the Pub/Sub topic | `string` | - | yes |
| message_retention_duration | Message retention duration for the topic | `string` | `"86400s"` | no |
| subscriptions | Map of subscription configurations | `map(object)` | `{}` | no |
| labels | Labels to apply to Pub/Sub resources | `map(string)` | `{}` | no |

### subscriptions object

| Name | Description | Type | Default |
|------|-------------|------|---------|
| ack_deadline_seconds | Acknowledgment deadline in seconds | `number` | `20` |
| message_retention_duration | Message retention duration | `string` | `"604800s"` |
| minimum_backoff | Minimum retry backoff | `string` | `"10s"` |
| maximum_backoff | Maximum retry backoff | `string` | `"600s"` |

## Outputs

| Name | Description |
|------|-------------|
| topic_id | The ID of the Pub/Sub topic |
| topic_name | The name of the Pub/Sub topic |
| subscription_ids | Map of subscription name to subscription ID |

## Requirements

| Name | Version |
|------|---------|
| terraform | ~> 1.9 |
| google | ~> 6.0 |

## Attribution

This module was created following best practices from [terraform-skill](https://github.com/antonbabenko/terraform-skill) by Anton Babenko.
