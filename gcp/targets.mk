.PHONY: test download download_terraform terraform build deploy

test:
	stack test

build: gcp/appEngine/app_engine_deployment.zip

deploy: gcp/appEngine/app_engine_deployment.zip
	mkdir -p gcp/deployments
ifndef VERSION
	$(error VERSION is undefined)
endif
	cp gcp/appEngine/app_engine_deployment.zip gcp/deployments/app_engine_deployment_${VERSION}.zip
	gsutil rsync -c gcp/deployments gs://${DEPLOYMENT_BUCKET}/builds

download: resources output download_terraform

download_terraform: gcp/secrets/config/terraform-backend.conf gcp/secrets/config/terraform-custom.tvars

secrets:
	mkdir -p gcp/secrets

gcp/secrets/config/terraform-backend.conf: | secrets
ifndef CONFIG_BUCKET
	$(error CONFIG_BUCKET is undefined)
endif
	gsutil -q cp gs://${CONFIG_BUCKET}/terraform/terraform-backend.conf gcp/secrets/config/terraform-backend.conf

gcp/secrets/config/terraform-custom.tvars: | secrets
ifndef CONFIG_BUCKET
	$(error CONFIG_BUCKET is undefined)
endif
	gsutil -q cp gs://${CONFIG_BUCKET}/terraform/terraform-custom.tvars gcp/secrets/config/terraform-custom.tvars

 
resources:
ifndef RESOURCE_BUCKET
	$(error RESOURCE_BUCKET is undefined)
endif
	mkdir -p resources
	gsutil -q cp -r gs://${RESOURCE_BUCKET}/marklight resources
	gsutil -q cp -r gs://${RESOURCE_BUCKET}/json resources

output:
ifndef RESOURCE_BUCKET
	$(error RESOURCE_BUCKET is undefined)
endif
	mkdir -p output
	gsutil -q rsync -r gs://${RESOURCE_BUCKET}/static output

output/index.html: | output resources
	stack exec website

gcp/appEngine/content: output/index.html
	cp -r output/. gcp/appEngine/content

gcp/appEngine/app_engine_deployment.zip: | gcp/appEngine/content
	find gcp/appEngine -exec touch -t 202204251730 {} +
	cd gcp/appEngine; zip -o -r --no-dir-entries app_engine_deployment.zip .

