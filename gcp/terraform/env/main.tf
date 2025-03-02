provider "google" {
  project = var.project
  zone    = var.zone
}

module "website" {
  source  = "../modules/app_engine"
  deployment_bucket = var.deployment_bucket
  project = var.project
  region  = var.region
  all_versions = ["latest", "2209", "2311", "2404", "2503"]
  latest_version = "2503"
}
