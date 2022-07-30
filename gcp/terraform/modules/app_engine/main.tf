locals {
	deploy_zip = "../../appEngine/app_engine_deployment.zip"
}

resource "random_string" "zip_id" {
  length = 10
  special = false
  upper = false
  keepers = {
    sha256 = filesha256(local.deploy_zip)
  }
}

resource "google_storage_bucket_object" "deployment-zip" {
  name   = "${random_string.zip_id.result}_app_engine_deployment.zip"
  bucket = google_storage_bucket.deployment-bucket.name
  source = local.deploy_zip 
}

resource "google_storage_bucket" "deployment-bucket" {
  name     = "${var.project}-deployment"
  location = var.region
  uniform_bucket_level_access = true
}
