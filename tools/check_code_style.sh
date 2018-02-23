#!/usr/bin/env bash

set -e

pushd $(dirname "${0}") > /dev/null
SCRIPTDIR=$(pwd -L)
popd > /dev/null

SOURCE_DIR=$(readlink -f "$SCRIPTDIR/../source/xen")

echo "Checking source in directory: $SOURCE_DIR"

cd $SOURCE_DIR

################################################################################
# Check includes are sensisble
#
# .hpp should be included by full path, since they are part of public interface
# of xeno-engine - and we expect xeno-engine to be compiled with its own include
# directory in the include path
#
# This ensures the include directory can be generated by simply fishing out all
# the hpp files from the main source directory leaving the cpp and hxx files
# behind
cmd="grep --color -R '#include \".*hpp\"'"
if [[ $(eval $cmd) ]] ; then
		printf "\n\n\n"
		printf "=====================================================\n"
		printf "The following files should be included non-relatively\n"
		printf "=====================================================\n"
		eval $cmd
		printf "\n"
		printf "=====================================================\n"
fi

# .hxx files on the other hand should be included relatively.
# They are an implementation detail and hence cannot be guarrentied to be
# in the xeno engine include path, eg, if we seperate out the source and
# includes into seperate directories.
#
# The relative structure of the files should however hold under such an
# operation
cmd="grep --color -R '#include <.*hxx>'"
if [[ $(eval $cmd) ]] ; then
		printf "\n\n\n"
		printf "=====================================================\n"
		printf "The following files should be included relatively\n"
		printf "=====================================================\n"
		eval $cmd
		printf "\n"
		printf "=====================================================\n"
fi
################################################################################


################################################################################
# Check for common mis-spellings
cmd="egrep --color -R -i 'verticies|indicies|matricies'"

if [[ $(eval $cmd) ]] ; then
		printf "\n\n\n"
		printf "=====================================================\n"
		printf "The following spelling mistakes were found\n"
		printf "=====================================================\n"
		eval $cmd
		printf "=====================================================\n"
fi
################################################################################
