{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hap.Dictionary.FieldFormI where
	
-- , ExistentialQuantification, RankNTypes, ScopedTypeVariables, RecordWildCards, LambdaCase	

import Hap.Dictionary.DicTypes
import Import_
--import Database.Persist(PersistEntity)

instance (PersistEntity e) => FieldForm e Text where
    fieldAForm _ = areq textField
    fieldForm  _ = mreq textField

instance (PersistEntity e) => FieldForm e (Maybe Text) where
    fieldAForm _ = aopt textField
    fieldForm  _ = mopt textField
