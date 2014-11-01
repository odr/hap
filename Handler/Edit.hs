{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Edit(getEditR, postEditR, deleteEditR) where
    
import Hap.Dictionary.Types(SomeDictionary)
import qualified Hap.Dictionary.EditHandler as EH
import Import_
import Foundation

getEditR :: SomeDictionary App -> PersistValue -> HandlerT App IO Html
getEditR = EH.getEditR

postEditR :: SomeDictionary App -> PersistValue -> HandlerT App IO Html
postEditR = EH.postEditR

deleteEditR :: SomeDictionary App -> PersistValue -> HandlerT App IO Html
deleteEditR = EH.deleteEditR
