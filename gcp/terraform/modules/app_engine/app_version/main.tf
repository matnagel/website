data "google_storage_bucket_object" "deployment_zip" {
  name   = "builds/${var.deployment_zip}"
  bucket = var.deployment_bucket
}

resource "random_pet" "blob_change" {
   length     = 2
   keepers    = { blob_hash = data.google_storage_bucket_object.deployment_zip.md5hash }
}

resource "google_app_engine_standard_app_version" "website_app" {
  version_id = "v-${var.app_version}-${random_pet.blob_change.id}"
  service    = "default"
  runtime    = "python310"
  project    = var.project

  entrypoint {
    shell = "gunicorn -b :$PORT -w 4 main:app"
  }

  deployment {
    zip {
      source_url = "https://storage.googleapis.com/${var.deployment_bucket}/builds/${var.deployment_zip}"
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

  instance_class = "B1"
  noop_on_destroy = true
}

output "version" {
  value = google_app_engine_standard_app_version.website_app.version_id
}

output "service" {
  value = google_app_engine_standard_app_version.website_app.service
}
