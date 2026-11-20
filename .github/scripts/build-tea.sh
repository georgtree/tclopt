#!/usr/bin/env bash
set -euo pipefail
root=$PWD
prefix="$root/_ci/runtime"
platform=unix
toolchain=linux-gcc
native_path() { printf '%s' "$1"; }
if [[ ${MSYSTEM:-} == UCRT64 ]]; then
    platform=win
    toolchain=windows-ucrt64
    native_path() { cygpath -m "$1"; }
fi
if [[ ${RUNTIME_CACHE_HIT:-false} != true ]]; then
    mkdir -p _ci/tcl-build
    cd _ci/tcl-build
    "$root/_ci/tcl/$platform/configure" --prefix="$prefix"
    make -j2
    make install
else
    echo "Using cached Tcl runtime"
fi
export PATH="$prefix/bin:$PATH"
export LD_LIBRARY_PATH="$prefix/lib:${LD_LIBRARY_PATH:-}"
extexpr="$root/_ci/extexpr-package"
if [[ ${EXTEXPR_CACHE_HIT:-false} != true ]]; then
    mkdir -p "$root/_ci/extexpr-build" "$extexpr"
    cd "$root/_ci/extexpr-build"
    bash "$root/_ci/extexpr/configure" --with-tcl="$prefix/lib" --prefix="$prefix"
    make -j2
    if [[ $platform == win ]]; then library=(*.dll); else library=(*.so); fi
    cp "${library[@]}" pkgIndex.tcl "$root/_ci/extexpr/extexpr.tcl" "$extexpr/"
fi
build="$root/_ci/opt-build"
mkdir -p "$build"
cd "$build"
"$root/configure" --with-tcl="$prefix/lib" --prefix="$prefix" --exec-prefix="$prefix"
make -j2
# pkgIndex.tcl loads the compiled library and sources tclopt.tcl beside it.
cp "$root/tclopt.tcl" .
export TCLLIBPATH="{$(native_path "$build")}"
pkg=$(cat "$root/_ci/argparse/package-dir.txt")
TCLLIBPATH+=" {$(native_path "$root/_ci/argparse/lib/$pkg")} {$(native_path "$extexpr")} {$(native_path "$root/_ci/tcllib/modules")}"
make test TCLLIBPATH="$TCLLIBPATH"
if [[ ${MAKE_DIST:-false} == true ]]; then
    ext=tar.gz
    target=dist
    if [[ $platform == win ]]; then ext=zip; target=dist-zip; fi
    make "$target" DIST_ROOT="$root/_ci/dist"
    archive=("$root"/_ci/dist/*."$ext")
    name=$(basename "${archive[0]}" ".$ext")
    mkdir -p "$root/_ci/package"
    cp "${archive[0]}" "$root/_ci/package/$name-$toolchain.$ext"
fi
