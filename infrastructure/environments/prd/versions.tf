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
  # cloudflare provider は api_token が null を許容しないため、未設定時は
  # ダミー値で provider init を通す。実際のリソース作成は
  # module "cloudflare_reader" (count = 0 when account_id is null) で抑止される。
  api_token = var.cloudflare_api_token == null ? "placeholder0000000000000000000000000000000" : var.cloudflare_api_token
}
