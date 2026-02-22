locals {
  environment = "prd"
  common_labels = {
    environment = local.environment
    managed_by  = "terraform"
    project     = "hut"
  }

  invoker_bindings = flatten([
    for member in var.authorized_members : [
      {
        key     = "admin-${member}"
        service = module.cloudrun_admin.service_name
        member  = member
      },
    ]
  ])
}

resource "google_project_iam_custom_role" "storage_object_delete" {
  role_id     = "storageObjectDelete"
  title       = "Storage Object Delete"
  description = "Allows listing, reading, and deleting storage objects"
  project     = var.project_id

  permissions = [
    "storage.objects.list",
    "storage.objects.get",
    "storage.objects.delete",
  ]
}

module "iam" {
  source = "../../modules/iam"

  depends_on = [google_project_iam_custom_role.storage_object_delete]

  project_id = var.project_id

  service_accounts = {
    "hut-prd-admin" = {
      display_name = "HUT PRD Admin Service Account"
      description  = "Service account for the PRD admin Cloud Run service"
      roles = [
        "roles/datastore.user",
        "roles/pubsub.publisher",
        "roles/secretmanager.secretAccessor",
      ]
    }
    "hut-prd-image-cleanup-worker" = {
      display_name = "HUT PRD Image Cleanup Worker Service Account"
      description  = "Service account for the PRD image cleanup worker"
      roles = [
        "roles/datastore.user",
        "projects/${var.project_id}/roles/storageObjectDelete",
        "roles/eventarc.eventReceiver",
      ]
    }
    "hut-prd-worker" = {
      display_name = "HUT PRD Worker Service Account"
      description  = "Service account for the PRD general worker"
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
  repository_id = "hut-prd"
  description   = "Docker images for HUT production environment"
  keep_count    = 10

  labels = local.common_labels
}

module "firestore" {
  source = "../../modules/firestore"

  project_id          = var.project_id
  location_id         = var.firestore_location
  database_name       = "(default)"
  delete_protection   = true
  enable_daily_backup = true
  backup_retention    = "604800s"
}

module "firebase_storage" {
  source = "../../modules/firebase_storage"

  project_id         = var.project_id
  bucket_name        = "${var.project_id}.firebasestorage.app"
  location           = var.region
  versioning_enabled = true

  labels = local.common_labels
}

module "secrets" {
  source = "../../modules/secret_manager"

  project_id = var.project_id

  secrets = {
    "prd-firebase-api-key" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-prd-admin"]}",
      ]
    }
    "prd-firebase-auth-domain" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-prd-admin"]}",
      ]
    }
    "prd-firebase-storage-bucket" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-prd-admin"]}",
      ]
    }
    "prd-firebase-messaging-sender-id" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-prd-admin"]}",
      ]
    }
    "prd-firebase-app-id" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-prd-admin"]}",
      ]
    }
    "prd-oidc-allowed-emails" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-prd-admin"]}",
      ]
    }
    "prd-event-pubsub-topic-name" = {
      accessor_members = [
        "serviceAccount:${module.iam.service_account_emails["hut-prd-admin"]}",
      ]
    }
  }

  labels = local.common_labels
}

module "pubsub_image_events" {
  source = "../../modules/pubsub"

  project_id = var.project_id
  topic_name = "image-events"

  labels = local.common_labels
}

module "pubsub_app_events" {
  source = "../../modules/pubsub"

  project_id = var.project_id
  topic_name = "app-events"

  labels = local.common_labels
}

module "cloudrun_admin" {
  source = "../../modules/cloudrun_service"

  project_id            = var.project_id
  region                = var.region
  service_name          = "lihs-hut-landlord"
  container_image       = var.admin_container_image
  service_account_email = module.iam.service_account_emails["hut-prd-admin"]
  allow_unauthenticated = false

  environment_variables = {
    NEXT_PUBLIC_FIREBASE_PROJECT_ID = var.project_id
    FIREBASE_PROJECT_ID             = var.project_id
  }

  labels = local.common_labels

  secret_environment_variables = [
    {
      name      = "NEXT_PUBLIC_FIREBASE_API_KEY"
      secret_id = module.secrets.secret_ids["prd-firebase-api-key"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN"
      secret_id = module.secrets.secret_ids["prd-firebase-auth-domain"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET"
      secret_id = module.secrets.secret_ids["prd-firebase-storage-bucket"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID"
      secret_id = module.secrets.secret_ids["prd-firebase-messaging-sender-id"]
      version   = "latest"
    },
    {
      name      = "NEXT_PUBLIC_FIREBASE_APP_ID"
      secret_id = module.secrets.secret_ids["prd-firebase-app-id"]
      version   = "latest"
    },
    {
      name      = "OIDC_ALLOWED_EMAILS"
      secret_id = module.secrets.secret_ids["prd-oidc-allowed-emails"]
      version   = "latest"
    },
    {
      name      = "EVENT_PUBSUB_TOPIC_NAME"
      secret_id = module.secrets.secret_ids["prd-event-pubsub-topic-name"]
      version   = "latest"
    },
  ]
}

module "cloudrun_image_cleanup_worker" {
  source = "../../modules/cloudrun_service"

  project_id            = var.project_id
  region                = var.region
  service_name          = "image-cleanup-worker"
  container_image       = var.image_cleanup_worker_container_image
  service_account_email = module.iam.service_account_emails["hut-prd-image-cleanup-worker"]
  allow_unauthenticated = false

  environment_variables = {
    FIREBASE_PROJECT_ID = var.project_id
  }

  labels = local.common_labels
}

module "cloudrun_worker" {
  source = "../../modules/cloudrun_service"

  project_id            = var.project_id
  region                = var.region
  service_name          = "worker"
  container_image       = var.worker_container_image
  service_account_email = module.iam.service_account_emails["hut-prd-worker"]
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

module "eventarc_image_cleanup" {
  source = "../../modules/eventarc_trigger"

  project_id            = var.project_id
  region                = var.region
  trigger_name          = "prd-image-cleanup-trigger"
  pubsub_topic_id       = module.pubsub_image_events.topic_id
  cloudrun_service_name = module.cloudrun_image_cleanup_worker.service_name
  service_account_email = module.iam.service_account_emails["hut-prd-image-cleanup-worker"]

  labels = local.common_labels
}

module "eventarc_worker" {
  source = "../../modules/eventarc_trigger"

  project_id            = var.project_id
  region                = var.region
  trigger_name          = "prd-worker-trigger"
  pubsub_topic_id       = module.pubsub_app_events.topic_id
  cloudrun_service_name = module.cloudrun_worker.service_name
  service_account_email = module.iam.service_account_emails["hut-prd-worker"]
  destination_path      = "/events"

  labels = local.common_labels
}

resource "google_cloud_run_v2_service_iam_member" "worker_invoker" {
  project  = var.project_id
  location = var.region
  name     = module.cloudrun_worker.service_name
  role     = "roles/run.invoker"
  member   = "serviceAccount:${module.iam.service_account_emails["hut-prd-worker"]}"
}
