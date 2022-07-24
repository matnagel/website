.PHONY: test download build deploy

test:
	stack test

build: gcp/appEngine/appEngine_deployment.zip

download: resources output gcp/secrets/config/terraform-backend.conf gcp/secrets/config/terraform-custom.tvars

deploy: gcp/appEngine/appEngine_deployment.zip

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
	mkdir resources
	gsutil -q cp -r gs://${RESOURCE_BUCKET}/marklight resources
	gsutil -q cp -r gs://${RESOURCE_BUCKET}/json resources

output:
ifndef RESOURCE_BUCKET
	$(error RESOURCE_BUCKET is undefined)
endif
	mkdir output
	gsutil -q cp -r gs://${RESOURCE_BUCKET}/static output

output/index.html: | output resources
	stack exec website

gcp/appEngine/contents: output/index.html
	cp -r output/. gcp/appEngine/contents

gcp/appEngine/appEngine_deployment.zip: | gcp/appEngine/contents
	find gcp/appEngine -exec touch -t 202204251730 {} +
	cd gcp/appEngine; zip -o -r appEngine_deployment.zip .

