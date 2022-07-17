.PHONY: test build check-env

test:
	stack test

build: output/index.html

resources:
ifndef RESOURCE_BUCKET
	$(error RESOURCE_BUCKET is undefined)
endif
	mkdir resources
	gsutil -q cp -r gs://${RESOURCE_BUCKET}/ resources
	ls -a
	ls -a resources

output: 
	mkdir output

output/index.html: | output resources
	stack exec website
