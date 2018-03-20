#!/usr/bin/env bash

set -e

pushd $(dirname "${0}") > /dev/null
SCRIPTDIR=$(pwd -L)
popd > /dev/null

SOURCE_DIR=$(readlink -f "$SCRIPTDIR/../source/xen")

echo "Checking source in directory: $SOURCE_DIR"

cd $SOURCE_DIR

section_has_been_printed=0
function check_section {
		section_name=$1
		cmd=$2

		if [[ $(eval $cmd) ]] ; then
				if [[ $section_has_been_printed == 1 ]] ; then
						printf "\n\n"
				fi
				section_has_been_printed=1
				printf "=====================================================\n"
				printf "$section_name\n"
				printf "=====================================================\n"
				eval $cmd
				printf "=====================================================\n"
		fi

}

# .hpp should be included by full path, since they are part of public interface
# of xeno-engine - and we expect xeno-engine to be compiled with its own include
# directory in the include path
#
# This ensures the include directory can be generated by simply fishing out all
# the hpp files from the main source directory leaving the cpp and hxx files
# behind
check_section "The following files should be included non-relatively" \
							"grep --color -R '#include \".*hpp\"'"

# .hxx files on the other hand should be included relatively.
# They are an implementation detail and hence cannot be guarrentied to be
# in the xeno engine include path, eg, if we seperate out the source and
# includes into seperate directories.
#
# The relative structure of the files should however hold under such an
# operation
check_section "The following files should be included relatively" \
							"grep --color -R '#include <.*hxx>'"


check_section "The following spelling mistakes were found" \
							"egrep --color -R -i 'verticies|indicies|matricies|primative'"

if [[ $section_has_been_printed == 0 ]] ; then
		printf "\033[1;32m--- No issues found ---\033[0m\n"
		exit 0
else
		printf "\033[1;31m*** Issues found ***\033[0m\n"
		exit 1
fi
