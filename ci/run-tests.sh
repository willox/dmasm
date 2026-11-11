#!/bin/sh
# Run binaries collected by ci/build-tests.sh:
#
#     ci/run-tests.sh DIRECTORY
#
# Run from the repository root, matching cargo's working directory.
# Set WINE=wine to run Windows binaries on Linux.
set -u
[ $# -eq 1 ] || { sed -n '2,8p' "$0" >&2; exit 2; }
[ -z "${WINE:-}" ] || export WINEDEBUG="${WINEDEBUG:--all}"

failed=
for test in "$1"/*; do
	name=$(basename "$test" .exe)
	echo "::group::$name"
	${WINE:+"$WINE"} "$test"
	status=$?
	echo "::endgroup::"
	if [ $status -eq 0 ]; then
		echo "passed: $name"
	else
		echo "FAILED: $name (exit status $status)"
		failed="$failed $name"
	fi
done

if [ -n "$failed" ]; then
	echo "failed:$failed"
	exit 1
fi
echo "all passed"
