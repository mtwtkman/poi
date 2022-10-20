module Poi.Operation.UpdateMetaInfo where

import Poi.Entity

getCurrentWholeMetaInfo :: TrashBox -> IO [MetaInfo]
getCurrentWholeMetaInfo trashbox = do
  content <- readFile $ metaInfoFileLocation trashbox
  let parsed = mapM deserialize $ lines content :: DeserializeResult [MetaInfo]
  case parsed of
    Right metainfos -> return metainfos
    Left DeserializeFailed -> return []
