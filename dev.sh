#!/bin/bash

cmd=$1
shift
case "${cmd}" in
  b|build) cabal build;;
  r|repl) cabal repl $@;;
  t|test) cabal test --test-show-details=always --test-options="--color always $@";;
  tp) ./dev.sh t "-p \"/$@/\"";;
  poi) POI_TRASH_CAN_PATH=`pwd`/test-poi-can cabal run poi $@;;
  *) cabal $@;;
esac
