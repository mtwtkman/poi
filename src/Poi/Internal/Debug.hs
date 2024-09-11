module Poi.Internal.Debug (tap)
where

tap :: (Show s) => String -> s -> IO s
tap label s = print (label <> " " <> show s) >> return s
