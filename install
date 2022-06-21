#!/usr/bin/env sh
rm -rf build
mkdir build \
&& ocamlopt \
	-nodynlink \
	-o $HOME/.local/bin/lfpreview unix.cmxa lfpreview.ml \
&& mv {*.cmi,*.cmx,*.o} build


