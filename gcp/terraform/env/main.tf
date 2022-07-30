provider "google" {
  project = var.project
  zone    = var.zone
}

module "website" {
  source  = "../modules/app_engine"
  project = var.project
  region  = var.region
}
