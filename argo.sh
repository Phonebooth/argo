#!/bin/bash
readonly ROOT=/opt/argo
readonly NITROGEN="$ROOT/nitrogen/bin/nitrogen"
function main() {
    local command="$1"
    "$NITROGEN" "$command"
}

main $@
