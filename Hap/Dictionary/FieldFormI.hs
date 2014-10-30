{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hap.Dictionary.FieldFormI where
	
-- , ExistentialQuantification, RankNTypes, ScopedTypeVariables, RecordWildCards, LambdaCase	

import Hap.Dictionary.Types(FieldForm(..))
import Import_
--import Database.Persist(PersistEntity)

instance (RenderMessage m FormMessage) => FieldForm m e Text where
    fieldAForm _ = areq textField
    fieldForm  _ = mreq textField

instance (RenderMessage m FormMessage) => FieldForm m e (Maybe Text) where
    fieldAForm _ = aopt textField
    fieldForm  _ = mopt textField

