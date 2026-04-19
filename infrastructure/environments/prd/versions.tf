terraform {
  required_version = "~> 1.14"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 7.0"
    }
    cloudflare = {
      source  = "cloudflare/cloudflare"
      version = "~> 4.52"
    }
  }

  backend "gcs" {
    bucket = "hut-prd-terraform-state"
    prefix = "terraform/state"
  }
}

provider "google" {
  project               = var.project_id
  region                = var.region
  user_project_override = true
}

provider "cloudflare" {
  api_token = var.cloudflare_api_token
}
