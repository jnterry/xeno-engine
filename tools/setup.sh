#!/usr/bin/env bash

set -e

if [[ -e /usr/bin/apt-get ]] ; then
		sudo apt-get install libgl1-mesa-dev libglu1-mesa-dev
fi

rm -rf ../build/
mkdir ../build/
cd ../build/
cmake ..
