resource "google_app_engine_application" "app_engine" {
  project     = var.project
  location_id = var.region
  lifecycle {
    prevent_destroy = true
  }
}

module "app_version" {
  source            = "./app_version"
  deployment_bucket = var.deployment_bucket
  deployment_zip    = "app_engine_deployment_latest.zip"
  project           = var.project
}

moved {
  from = google_app_engine_application.app-engine
  to   = google_app_engine_application.app_engine
}

moved {
  from = google_storage_bucket.deployment-bucket
  to   = google_storage_bucket.deployment_bucket
}

moved {
  from = google_app_engine_standard_app_version.website-app
  to   = module.app_version.google_app_engine_standard_app_version.website_app
}
