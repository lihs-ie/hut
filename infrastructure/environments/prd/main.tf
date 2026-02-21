locals {
  environment = "prd"
  common_labels = {
    environment = local.environment
    managed_by  = "terraform"
    project     = "hut"
  }
}

# ---------------------------------------------------------------------------
# IAM - Service Accounts
# ---------------------------------------------------------------------------
module "iam" {
  source = "../../modules/iam"

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
        "roles/storage.objectAdmin",
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

# ---------------------------------------------------------------------------
# Artifact Registry
# ---------------------------------------------------------------------------
module "artifact_registry" {
  source = "../../modules/artifact_registry"

  project_id    = var.project_id
  region        = var.region
  repository_id = "hut-prd"
  description   = "Docker images for HUT production environment"
  keep_count    = 10

  labels = local.common_labels
}

# ---------------------------------------------------------------------------
# Firestore
# ---------------------------------------------------------------------------
module "firestore" {
  source = "../../modules/firestore"

  project_id        = var.project_id
  location_id       = var.firestore_location
  database_name     = "(default)"
  delete_protection = true
}

# ---------------------------------------------------------------------------
# Firebase Storage
# ---------------------------------------------------------------------------
module "firebase_storage" {
  source = "../../modules/firebase_storage"

  project_id         = var.project_id
  bucket_name        = "${var.project_id}.firebasestorage.app"
  location           = var.region
  versioning_enabled = true

  labels = local.common_labels
}

# ---------------------------------------------------------------------------
# Secret Manager
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# Pub/Sub Topics
# ---------------------------------------------------------------------------
module "pubsub_image_events" {
  source = "../../modules/pubsub"

  project_id = var.project_id
  topic_name = "image-events"

  labels = local.common_labels
}

module "pubsub_article_events" {
  source = "../../modules/pubsub"

  project_id = var.project_id
  topic_name = "article-events"

  labels = local.common_labels
}

module "pubsub_app_events" {
  source = "../../modules/pubsub"

  project_id = var.project_id
  topic_name = "app-events"

  labels = local.common_labels
}

# ---------------------------------------------------------------------------
# Cloud Run Services (admin + workers only; reader is on Cloudflare Workers)
# ---------------------------------------------------------------------------
module "cloudrun_admin" {
  source = "../../modules/cloudrun_service"

  project_id            = var.project_id
  region                = var.region
  service_name          = "lihs-hut-landlord"
  container_image       = var.admin_container_image
  service_account_email = module.iam.service_account_emails["hut-prd-admin"]
  allow_unauthenticated = true

  environment_variables = {
    NEXT_PUBLIC_FIREBASE_PROJECT_ID = var.project_id
    FIREBASE_PROJECT_ID             = var.project_id
  }

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
}

# ---------------------------------------------------------------------------
# Eventarc Triggers
# ---------------------------------------------------------------------------
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

  labels = local.common_labels
}

# ---------------------------------------------------------------------------
# Cloudflare Workers (PRD reader)
# ---------------------------------------------------------------------------
# NOTE: The PRD reader is deployed via Wrangler CLI as part of the CI/CD
# pipeline. The Cloudflare Workers Script resource is managed here for
# reference and to ensure the account/project binding exists, but the actual
# deployment (code upload) is handled by Wrangler.
#
# Wrangler secrets for the reader are set via:
#   wrangler secret put NEXT_PUBLIC_FIREBASE_API_KEY
#   wrangler secret put NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN
#   wrangler secret put NEXT_PUBLIC_FIREBASE_PROJECT_ID
#   wrangler secret put NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET
#   wrangler secret put NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID
#   wrangler secret put NEXT_PUBLIC_FIREBASE_APP_ID
#   wrangler secret put NEXT_PUBLIC_SITE_URL
# ---------------------------------------------------------------------------
