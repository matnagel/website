.PHONY: test build

test:
	stack test

build: output/index.html

upload: gcp/appEngine/contents

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
	cp -r output/ gcp/appEngine/contents
	ls -a gcp/appEngine/contents
