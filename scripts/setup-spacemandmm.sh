#!/bin/sh
# setup-spacemandmm.sh [DIR]
#
# dmasm builds against SpacemanDMM's `dreammaker` crate with some parser fixes
# that aren't upstream yet. They live in patches/spacemandmm.patch, a `git am`
# series on SpaceManiac/SpacemanDMM a388985 ("Bump versions for suite 1.12").
# This clones SpacemanDMM into DIR (default: ../SpacemanDMM next to this
# repository, where Cargo.toml's [patch] looks) and applies the series.
#
# An existing DIR is left alone, so a checkout you're working in is safe.
set -eu

here=$(cd "$(dirname "$0")/.." && pwd)
dir=${1:-$here/../SpacemanDMM}
base=a388985

if [ -e "$dir" ]; then
	echo "$dir already exists; leaving it as it is"
	exit 0
fi

git clone --quiet https://github.com/SpaceManiac/SpacemanDMM "$dir"
git -C "$dir" checkout --quiet -b dmasm-patches "$base"
git -C "$dir" -c user.name=dmasm -c user.email=dmasm@localhost am --quiet "$here/patches/spacemandmm.patch"
echo "SpacemanDMM $base with $(grep -c '^Subject:' "$here/patches/spacemandmm.patch") patches applied in $dir"
