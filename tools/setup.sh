#!/usr/bin/env bash

set -e

# SFML dependencies
sudo apt install libxrandr-dev libudev-dev libgl-dev libjpeg-dev libopenal-dev libflac-dev

rm -rf ../build/
mkdir ../build/
cd ../build/
cmake ..
