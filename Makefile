.PHONY: 

PROJECT = argo

DEPS += pmod_transform
DEPS += lager
DEPS += thumper
dep_thumper = git git@github.com:Phonebooth/thumper.git r16

$(PROJECT).d:: deps
	$(MAKE) -C deps/pmod_transform

include erlang.mk

$(PROJECT).d::
	$(MAKE) -C nitrogen all DEPS_DIR="" REBAR_DEPS_DIR="" APPS_DIR=""

run:
	./scripts/argoctl console

run_start:
	./scripts/argoctl start

#clean:
#	cd nitrogen && $(MAKE) clean
#
#
