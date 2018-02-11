#!/usr/bin/env bash

set -e

pushd $(dirname "${0}") > /dev/null
SCRIPTDIR=$(pwd -L)
popd > /dev/null

cd $SCRIPTDIR

rm -rf ../docs
mkdir ../docs

doxygen
