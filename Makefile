.PHONY: all clean run clean_site clean_nitrogen argo build_nitrogen
.INTERMEDIATE: nitrogen

all: nitrogen argo

argo: build_nitrogen

build_nitrogen:
	cd nitrogen && $(MAKE)

nitrogen: nitrogen/bin/nitrogen

nitrogen/bin/nitrogen: build
	cd build && tar xfz ../files/nitrogen-* && cd ..
	rm -rf build/nitrogen/site
	mv build/nitrogen/* nitrogen

build:
	mkdir build

run:
	./scripts/argoctl

clean:
	find nitrogen -mindepth 1 -maxdepth 1 -not -name site -exec rm -rf "{}" \;
	cd nitrogen && $(MAKE) clean
