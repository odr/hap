module Handler.List(getListR, postListR) where

import Hap.Dictionary.Types(SomeDictionary)
import qualified  Hap.Dictionary.ListHandler as LH
import Import_
import Foundation

getListR :: SomeDictionary App -> HandlerT App IO Html
getListR = LH.getListR

postListR :: SomeDictionary App -> HandlerT App IO Html
postListR = LH.postListR

