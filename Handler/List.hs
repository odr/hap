module Handler.List(getListR, postListR) where

import Import
import qualified  Hap.Dictionary.ListHandler as LH

getListR :: String -> HandlerT App IO Html
getListR = LH.getListR . read

postListR :: String -> HandlerT App IO Html
postListR = LH.postListR . read

