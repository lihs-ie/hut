locals {
  environment = "stg"
  common_labels = {
    environment = local.environment
    managed_by  = "terraform"
    project     = "hut"
  }

  invoker_bindings = flatten([
    for member in var.authorized_members : [
      {
        key     = "reader-${member}"
        service = module.cloudrun_reader.service_name
        member  = member
      },
      {
        key     = "admin-${member}"
        service = module.cloudrun_admin.service_name
        member  = member
      },
    ]
  ])
}

module "iam" {
  source = "../../modules/iam"

  project_id = var.project_id

  service_accounts = {
    "hut-stg-reader" = {
      display_name = "HUT STG Reader Service Account"
      description  = "Service account for the STG reader Cloud Run service"
      roles = [
        "roles/datastore.user",
      ]
    }
    "hut-stg-admin" = {
      display_name = "HUT STG Admin Service Account"
      description  = "Service account for the STG admin Cloud Run service"
      roles = [
        "roles/datastore.user",
        "roles/pubsub.publisher",
        "roles/secretmanager.secretAccessor",
        "roles/storage.objectAdmin",
      ]
    }
    "hut-stg-search-token-worker" = {
      display_name = "HUT STG Search Token Worker Service Account"
      description  = "Service account for the STG search token worker"
      roles = [
        "roles/datastore.user",
        "roles/eventarc.eventReceiver",
      ]
    }
  }
}

module "artifact_registry" {
  source = "../../modules/artifact_registry"

  project_id    = var.project_id
  region        = var.region
  repository_id = "hut-stg"
  description   = "Docker images for HUT staging environment"
  keep_count    = 5

  labels = local.common_labels
}

module "firestore" {
  source = "../../modules/firestore"

  project_id    = var.project_id
  location_id   = var.firestore_location
  database_name = "(default)"
}

module "firebase_storage" {
  source = "../../modules/firebase_storage"

  project_id  = var.project_id
  bucket_name = "${var.project_id}.firebasestorage.app"
  location    = "US-EAST1"

  labels = local.common_labels
}

module "secrets" {
  source = "../../modules/secret_manager"

  project_id = var.project_id

  secrets = {
    "stg-firebase-api-key" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-stg-reader"]}",
        "serviceAccount:${module.iam.service_account_emails["hut-stg-admin"]}",
      ]
    }
    "stg-firebase-auth-domain" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-stg-reader"]}",
        "serviceAccount:${module.iam.service_account_emails["hut-stg-admin"]}",
      ]
    }
    "stg-firebase-storage-bucket" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-stg-reader"]}",
        "serviceAccount:${module.iam.service_account_emails["hut-stg-admin"]}",
      ]
    }
    "stg-firebase-messaging-sender-id" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-stg-reader"]}",
        "serviceAccount:${module.iam.service_account_emails["hut-stg-admin"]}",
      ]
    }
    "stg-firebase-app-id" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-stg-reader"]}",
        "serviceAccount:${module.iam.service_account_emails["hut-stg-admin"]}",
      ]
    }
    "stg-oidc-allowed-emails" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-stg-admin"]}",
      ]
    }
    "stg-event-pubsub-topic-name" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-stg-admin"]}",
      ]
    }
  }

  labels = local.common_labels
}

module "pubsub_app_events" {
  source = "../../modules/pubsub"

  project_id = var.project_id
  topic_name = "app-events"

  labels = local.common_labels
}

module "cloudrun_reader" {
  source = "../../modules/cloudrun_service"

  project_id            = var.project_id
  region                = var.region
  service_name          = "stg-lihs-hut"
  container_image       = var.reader_container_image
  service_account_email = module.iam.service_account_emails["hut-stg-reader"]
  allow_unauthenticated = false

  environment_variables = {
    NEXT_PUBLIC_FIREBASE_PROJECT_ID = var.project_id
    DISALLOW_ROBOTS                 = "true"
  }

  labels = local.common_labels

  secret_environment_variables = [
    {
      name      = "NEXT_PUBLIC_FIREBASE_API_KEY"
      secret_id = module.secrets.secret_ids["stg-firebase-api-key"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN"
      secret_id = module.secrets.secret_ids["stg-firebase-auth-domain"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET"
      secret_id = module.secrets.secret_ids["stg-firebase-storage-bucket"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID"
      secret_id = module.secrets.secret_ids["stg-firebase-messaging-sender-id"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_APP_ID"
      secret_id = module.secrets.secret_ids["stg-firebase-app-id"]
      version   = "latest"
    },
  ]
}

module "cloudrun_admin" {
  source = "../../modules/cloudrun_service"

  project_id            = var.project_id
  region                = var.region
  service_name          = "stg-lihs-hut-landlord"
  container_image       = var.admin_container_image
  service_account_email = module.iam.service_account_emails["hut-stg-admin"]
  allow_unauthenticated = false

  environment_variables = {
    NEXT_PUBLIC_FIREBASE_PROJECT_ID = var.project_id
    FIREBASE_PROJECT_ID             = var.project_id
    DISALLOW_ROBOTS                 = "true"
    DISABLE_SECURE_COOKIE           = "true"
  }

  labels = local.common_labels

  secret_environment_variables = [
    {
      name      = "NEXT_PUBLIC_FIREBASE_API_KEY"
      secret_id = module.secrets.secret_ids["stg-firebase-api-key"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN"
      secret_id = module.secrets.secret_ids["stg-firebase-auth-domain"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET"
      secret_id = module.secrets.secret_ids["stg-firebase-storage-bucket"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID"
      secret_id = module.secrets.secret_ids["stg-firebase-messaging-sender-id"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_APP_ID"
      secret_id = module.secrets.secret_ids["stg-firebase-app-id"]
      version   = "latest"
    },
    {
      name      = "OIDC_ALLOWED_EMAILS"
      secret_id = module.secrets.secret_ids["stg-oidc-allowed-emails"]
      version   = "latest"
    },
    {
      name      = "EVENT_PUBSUB_TOPIC_NAME"
      secret_id = module.secrets.secret_ids["stg-event-pubsub-topic-name"]
      version   = "latest"
    },
  ]
}

module "cloudrun_search_token_worker" {
  source = "../../modules/cloudrun_service"

  project_id            = var.project_id
  region                = var.region
  service_name          = "stg-search-token-worker"
  container_image       = var.search_token_worker_container_image
  service_account_email = module.iam.service_account_emails["hut-stg-search-token-worker"]
  allow_unauthenticated = false

  environment_variables = {
    FIREBASE_PROJECT_ID = var.project_id
  }

  labels = local.common_labels
}

resource "google_cloud_run_v2_service_iam_member" "invoker" {
  for_each = { for binding in local.invoker_bindings : binding.key => binding }

  project  = var.project_id
  location = var.region
  name     = each.value.service
  role     = "roles/run.invoker"
  member   = each.value.member
}

module "eventarc_search_token_worker" {
  source = "../../modules/eventarc_trigger"

  project_id            = var.project_id
  region                = var.region
  trigger_name          = "stg-search-token-worker-trigger"
  pubsub_topic_id       = module.pubsub_app_events.topic_id
  cloudrun_service_name = module.cloudrun_search_token_worker.service_name
  service_account_email = module.iam.service_account_emails["hut-stg-search-token-worker"]
  destination_path      = "/events"

  labels = local.common_labels
}

resource "google_cloud_run_v2_service_iam_member" "search_token_worker_invoker" {
  project  = var.project_id
  location = var.region
  name     = module.cloudrun_search_token_worker.service_name
  role     = "roles/run.invoker"
  member   = "serviceAccount:${module.iam.service_account_emails["hut-stg-search-token-worker"]}"
}

module "github_actions_iam" {
  source = "../../modules/github_actions_iam"

  project_id        = var.project_id
  github_repository = var.github_repository
}

module "billing_export" {
  source = "../../modules/billing_export"

  project_id                      = var.project_id
  location                        = var.billing_export_location
  billing_account_service_account = var.billing_export_service_account

  labels = local.common_labels
}
