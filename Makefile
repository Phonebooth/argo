.PHONY: ng ng-download ng-unpack ng-build ng-put ng-clean

PROJECT = argo

DEPS_DIR = /home/jstimpson/argo/deps

DEPS += pmod_transform
DEPS += lager
DEPS += thumper
dep_thumper = git git@github.com:Phonebooth/thumper.git r16

$(PROJECT).d:: deps
	$(MAKE) -C deps/pmod_transform

include erlang.mk

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
	rm -rf build/nitrogen/argo/etc

ng-put:
	mv build/nitrogen/argo/* nitrogen

ng-clean:
	rm -rf build
	find nitrogen -mindepth 1 -maxdepth 1 \
		-not -name site -and \
		-not -name etc -exec rm -rf "{}" \;

$(PROJECT).d::
	$(MAKE) -C nitrogen all DEPS_DIR="" REBAR_DEPS_DIR="" APPS_DIR=""

run:
	./scripts/argoctl

#clean:
#	cd nitrogen && $(MAKE) clean
