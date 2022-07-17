.PHONY: test output check-env

test:
	stack test

output: output/index.html

resource: check-env
	gsutil -qr cp gs://${RESOURCE_BUCKET}/ resource
	ls -a resource

output/index.html: resource
	mkdir output
	stack exec webpage

check-env:
ifndef RESOURCE_BUCKET
	$(error RESOURCE_BUCKET is undefined)
endif

