#!/bin/sh

set -e
cd terraform/env
terraform init -backend-config="../../secrets/config/terraform-backend.conf"
terraform plan -var-file="../../secrets/config/terraform-custom.tvars" -out deployment-plan
#terraform apply deployment-plan
