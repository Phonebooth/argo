.PHONY: all clean run clean_site clean_nitrogen argo build_nitrogen
.INTERMEDIATE: nitrogen

all: nitrogen nitrogen/site argo

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

nitrogen/site:
	ln -s ../site nitrogen/site

run:
	./scripts/argoctl

clean:
	cd nitrogen && $(MAKE) clean
