.PHONY: all clean run clean_site clean_nitrogen argo build_nitrogen
.INTERMEDIATE: nitrogen

all: nitrogen nitrogen/site argo

argo: build_nitrogen

build_nitrogen:
	cd nitrogen && $(MAKE)

nitrogen: nitrogen/bin/nitrogen

nitrogen/bin/nitrogen:
	tar xfz files/nitrogen-*
	rm -rf nitrogen/site

nitrogen/site:
	ln -s ../site nitrogen/site

run:
	./scripts/argoctl

clean: clean_site clean_nitrogen

clean_site:
	rm -f nitrogen/site
	
clean_nitrogen:
	rm -rf nitrogen
