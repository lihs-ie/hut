resource "google_eventarc_trigger" "this" {
  name     = var.trigger_name
  location = var.region
  project  = var.project_id

  matching_criteria {
    attribute = "type"
    value     = "google.cloud.pubsub.topic.v1.messagePublished"
  }

  destination {
    cloud_run_service {
      service = var.cloudrun_service_name
      region  = var.region
      path    = var.destination_path
    }
  }

  transport {
    pubsub {
      topic = var.pubsub_topic_id
    }
  }

  service_account = var.service_account_email

  labels = var.labels

  lifecycle {
    ignore_changes = [transport]
  }
}
