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
  bucket = var.deployment-bucket
  source = var.deployment-zip
}

resource "google_app_engine_standard_app_version" "website_app" {
  version_id = "v-${random_string.zip_id.result}"
  service    = "default"
  runtime    = "python310"

  entrypoint {
    shell = "gunicorn -b :$PORT -w 4 main:app"
  }

  deployment {
    zip {
      source_url = "https://storage.googleapis.com/${var.deployment_bucket.name}/${var.deployment_zip.name}"
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

output "version" {
  value = google_app_engine_standard_app_version.website_app.version_id
}

output "service" {
  value = google_app_engine_standard_app_version.website-app.service
}
