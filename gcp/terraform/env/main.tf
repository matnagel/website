provider "google" {
  project = var.project
  zone    = var.zone
}

module "website" {
  source  = "../modules/website"
  project = var.project
  region  = var.region
}
