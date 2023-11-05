provider "google" {
  project = var.project
  zone    = var.zone
}

module "website" {
  source  = "../modules/app_engine"
  deployment_bucket = var.deployment_bucket
  project = var.project
  region  = var.region
  all_versions = ["2209", "2311"]
  latest_version = "2311"
}
