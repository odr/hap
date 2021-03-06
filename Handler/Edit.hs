{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Edit(getEditR, postEditR, deleteEditR) where

import Import
import Prelude(read)
import qualified Hap.Dictionary.EditHandler as EH

getEditR :: String -> PersistValue -> HandlerT App IO Html
getEditR = EH.getEditR . read

postEditR :: String -> PersistValue -> HandlerT App IO Html
postEditR = EH.postEditR . read

deleteEditR :: String -> PersistValue -> HandlerT App IO Value
deleteEditR = EH.deleteEditR . read
