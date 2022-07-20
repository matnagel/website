.PHONY: test download build deploy

test:
	stack test

build: gcp/appEngine/contents

download: resources output

deploy: gcp/appEngine/contents

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
	find gcp/appEngine -exec touch -t 202204251730 {} +
	cd gcp/appEngine; zip -o -r appEngine_deployment.zip .

