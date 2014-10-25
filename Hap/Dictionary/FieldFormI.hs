{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hap.Dictionary.FieldFormI where
	
-- , ExistentialQuantification, RankNTypes, ScopedTypeVariables, RecordWildCards, LambdaCase	

import Hap.Dictionary.DicTypes(HasDictionary(..), FieldForm(..))
import Import_
--import Database.Persist(PersistEntity)

instance (HasDictionary m e) => FieldForm m e Text where
    fieldAForm _ = areq textField
    fieldForm  _ = mreq textField

instance (HasDictionary m e) => FieldForm m e (Maybe Text) where
    fieldAForm _ = aopt textField
    fieldForm  _ = mopt textField

