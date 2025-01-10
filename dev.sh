#!/bin/bash

function current_version()
{
  rg '^version:\s*\d+\.\d+\.\d+$' ./poi.cabal | cut -d ':' -f 2 | xargs echo -n
}

function put_version_file()
{
  version="$(current_version)"
  major="$(echo ${version} | cut -d '.' -f 1)"
  minor="$(echo ${version} | cut -d '.' -f 2)"
  micro="$(echo ${version} | cut -d '.' -f 3)"

  cat <<EOF > ./src/Poi/Version.hs
module Poi.Version (version, Version) where

import Data.List (intercalate)

data Version = Version
  { major :: Int
  , minor :: Int
  , micro :: Int
  }
  deriving (Eq)

instance Show Version where
  show (Version{major = x, minor = y, micro = z}) = intercalate "." $ map show [x, y, z]

version :: Version
version = Version ${major} ${minor} ${micro}
EOF
}

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
    put_version_file
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
