#!/bin/bash

cmd=$1
shift
case "${cmd}" in
  b|build) cabal build;;
  r|repl) cabal repl $@;;
  run) cabal run poi -- $@;;
  t|test) cabal test --test-show-details=always --test-options="--color always $@";;
  tp) ./dev.sh t "-p \"/$@/\"";;
  poi) POI_TRASH_CAN_PATH=`pwd`/test-poi-can cabal run poi $@;;
  release)
    destdir="dest"
    out="${destdir}/poi"
    if [ ! -d "${destdir}" ]; then
      mkdir "${destdir}"
    fi
    cabal build
    cp "$(cabal list-bin poi)" "${out}"
    chmod +x "${out}"
    echo "Generated '${out}'"
    ;;
  *) cabal $@;;
esac
