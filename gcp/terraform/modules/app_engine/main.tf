locals {
  deploy_zip = "../../appEngine/app_engine_deployment.zip"
}

resource "random_string" "zip_id" {
  length  = 10
  special = false
  upper   = false
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
  name                        = "${var.project}-deployment"
  location                    = var.region
  uniform_bucket_level_access = true
  versioning {
    enabled = true
  }
}

resource "google_app_engine_standard_app_version" "website-app" {
  version_id = random_string.zip_id.result
  service    = "default"
  runtime    = "python310"

  entrypoint {
    shell = "gunicorn -b :$PORT -w 4 main:app"
  }

  deployment {
    zip {
      source_url = "https://storage.googleapis.com/${google_storage_bucket.deployment-bucket.name}/${google_storage_bucket_object.deployment-zip.name}"
    }
  }

  basic_scaling {
    max_instances = 2
  }

  lifecycle {
    create_before_destroy = true
  }
}

resource "google_app_engine_application" "app-engine" {
  project     = var.project
  location_id = var.region
}

resource "google_app_engine_service_split_traffic" "website-traffic" {
  service         = google_app_engine_standard_app_version.website-app.service
  migrate_traffic = true
  split {
    shard_by = "IP"
    allocations = {
      (google_app_engine_standard_app_version.website-app.version_id) = 1
    }
  }
}
