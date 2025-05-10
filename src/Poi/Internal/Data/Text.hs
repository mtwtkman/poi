{-# LANGUAGE OverloadedStrings #-}
module Poi.Internal.Data.Text (concatText) where

import Data.Text (Text, intercalate)

concatText :: [Text] -> Text
concatText = intercalate ""
