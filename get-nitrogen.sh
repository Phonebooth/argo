#!/bin/bash
function ng-clean {
    find build/nitrogen -mindepth 1 -maxdepth 1 \
        -not -name 'nitrogen-*.tar.gz' \
        -exec rm -rf "{}" \;
    find nitrogen -mindepth 1 -maxdepth 1 \
        -not -name site -and \
        -not -name etc -exec rm -rf "{}" \;
}

function ng-download {
    local file;
    file=build/nitrogen/nitrogen-2.3.1.tar.gz

    mkdir -p build/nitrogen
    if [ ! -f "$file" ]
    then
        wget --no-check-certificate \
            -O "$file" \
            https://github.com/nitrogen/nitrogen/tarball/v2.3.1
    fi
}

function ng-unpack {
    tar xfz build/nitrogen/nitrogen-2.3.1.tar.gz -C build/nitrogen
    rm -rf build/nitrogen/nitrogen-2.3.1
    mv build/nitrogen/nitrogen-nitrogen* build/nitrogen/nitrogen-2.3.1
}

function ng-build {
    cd build/nitrogen/nitrogen-2.3.1 && \
        make rel_webmachine PROJECT=argo && \
        cd ../../..
    rm -rf build/nitrogen/argo/site
    rm -rf build/nitrogen/argo/etc
}

function ng-put {
    echo $(pwd)
    mv build/nitrogen/argo/* nitrogen
}

function main {
    ng-clean
    ng-download
    ng-unpack
    ng-build
    ng-put
}

main
