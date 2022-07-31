resource "random_string" "zip_id" {
  length  = 10
  special = false
  upper   = false
  keepers = {
    sha256 = filesha256(var.deployment_zip)
  }
}

resource "google_storage_bucket_object" "deployment_zip" {
  name   = "${random_string.zip_id.result}_app_engine_deployment.zip"
  bucket = var.deployment_bucket
  source = var.deployment_zip
}

resource "google_app_engine_standard_app_version" "website_app" {
  version_id = "v-${random_string.zip_id.result}"
  service    = "default"
  runtime    = "python310"
  project = var.project

  entrypoint {
    shell = "gunicorn -b :$PORT -w 4 main:app"
  }

  deployment {
    zip {
      source_url = "https://storage.googleapis.com/${var.deployment_bucket}/${google_storage_bucket_object.deployment_zip.name}"
    }
  }

  handlers {
    url_regex      = ".*"
    security_level = "SECURE_ALWAYS"
    script {
      script_path = "auto"
    }
  }

  basic_scaling {
    max_instances = 2
  }

  lifecycle {
    create_before_destroy = true
  }
}

resource "google_app_engine_service_split_traffic" "traffic" {
  service         = google_app_engine_standard_app_version.website_app.service
  migrate_traffic = false
  split {
    shard_by = "IP"
    allocations = {
      (google_app_engine_standard_app_version.website_app.version_id) = 1
    }
  }
}

output "version" {
  value = google_app_engine_standard_app_version.website_app.version_id
}

output "service" {
  value = google_app_engine_standard_app_version.website_app.service
}