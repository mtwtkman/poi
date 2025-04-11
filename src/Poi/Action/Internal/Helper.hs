module Poi.Action.Internal.Helper
  (duplicationSafeName)
where

duplicationSafeName :: String -> String
duplicationSafeName = (".poi.pickup." <>)
