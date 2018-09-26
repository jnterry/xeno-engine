#!/usr/bin/env bash

set -e

pushd $(dirname "${0}") > /dev/null
SCRIPTDIR=$(pwd -L)
popd > /dev/null

SOURCE_DIR=$(readlink -f "$SCRIPTDIR/../source")

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
check_section "Public include files should never include private include files" \
							"find . -type f -regex '.*/include/.*.hpp' | xargs grep --color -n '#include.*hxx'"


check_section "Relative includes containing .. is messy" \
							"grep --color -n -R '#include \".*\.\..*\"'"


check_section "The following spelling mistakes were found" \
							"egrep --color -R -i 'verticies|indicies|matricies|primative'"

check_section "Log calls should not have trailing new line" \
							"egrep --color -R -i 'XenLog[A-Za-z]+\(\".*\\\\n\"'"

if [[ $section_has_been_printed == 0 ]] ; then
		printf "\033[1;32m--- No issues found ---\033[0m\n"
		exit 0
else
		printf "\033[1;31m*** Issues found ***\033[0m\n"
		exit 1
fi
