#!/usr/bin/env bash

set -e

pushd $(dirname "${0}") > /dev/null
SCRIPTDIR=$(pwd -L)
popd > /dev/null

SOURCE_DIR=$(readlink -f "$SCRIPTDIR/../source")

cd $SOURCE_DIR

EXCLUDE_STRING="*~"

grep --color -R ":TODO:"  --exclude ${EXCLUDE_STRING}

printf "Found %i todos\n" $(grep -R ":TODO:" --exclude ${EXCLUDE_STRING} | wc -l)
