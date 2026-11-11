#!/bin/bash
# Build and collect test binaries for another runner:
#
#     ci/build-tests.sh DIRECTORY [CARGO TEST'S ARGUMENTS...]
#
# ci/run-tests.sh runs them.
set -euo pipefail
[ $# -ge 1 ] || { sed -n '2,7p' "$0" >&2; exit 2; }
dest=$1
shift
cargo test --no-run --message-format=json "$@" | python3 -c '
import json, os, shutil, sys

dest = sys.argv[1]
for line in sys.stdin:
	try:
		message = json.loads(line)
	except ValueError:
		continue
	if message.get("reason") != "compiler-artifact" or not message["profile"]["test"] or not message.get("executable"):
		continue
	name = message["target"]["name"] + os.path.splitext(message["executable"])[1]
	os.makedirs(dest, exist_ok=True)
	shutil.copy(message["executable"], os.path.join(dest, name))
	print("tests:", name)
' "$dest"
