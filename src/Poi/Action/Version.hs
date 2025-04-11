module Poi.Action.Version (showCurrentVersion) where

import Poi.Version (Version, version)

showCurrentVersion :: IO Version
showCurrentVersion = return version
