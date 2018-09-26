#!/usr/bin/env bash

set -e

pushd $(dirname "${0}") > /dev/null
SCRIPTDIR=$(pwd -L)
popd > /dev/null

SOURCE_DIR=$(readlink -f "$SCRIPTDIR/../source")

cd $SOURCE_DIR

grep --color -R ":TODO:"

printf "Found %i todos\n" $(grep -R ":TODO:" | wc -l)
