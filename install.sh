#!/usr/bin/env sh
rm -rf build
mkdir build \
&& ocamlopt -color auto -o $HOME/.local/bin/lfpreview unix.cmxa lfpreview.ml \
&& mv {*.cmi,*.cmx,*.o} build
