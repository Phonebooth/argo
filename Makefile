.PHONY: ng ng-download ng-unpack ng-build ng-put ng-clean

all: argo

ng: ng-download ng-unpack ng-build ng-put

ng-download:
	mkdir -p build/nitrogen
	wget --no-check-certificate \
		-O build/nitrogen/nitrogen-2.3.1.tar.gz \
		https://github.com/nitrogen/nitrogen/tarball/v2.3.1

ng-unpack:
	tar xfz build/nitrogen/nitrogen-2.3.1.tar.gz -C build/nitrogen
	mv build/nitrogen/nitrogen-nitrogen* build/nitrogen/nitrogen-2.3.1

ng-build:
	cd build/nitrogen/nitrogen-2.3.1 && make rel_webmachine PROJECT=argo
	rm -rf build/nitrogen/argo/site

ng-put:
	mv build/nitrogen/argo/* nitrogen

ng-clean:
	rm -rf build
	find nitrogen -mindepth 1 -maxdepth 1 -not -name site -exec rm -rf "{}" \;

argo:
	cd nitrogen && $(MAKE) all

run:
	./scripts/argoctl

clean:
	cd nitrogen && $(MAKE) clean
