{-# LANGUAGE RecordWildCards #-}
module Hap.Dictionary.EDSL
	( module Hap.Dictionary.EDSL
	, HasDictionary(..)
	, SomeDictionary(..)
	) where

import Import_
import Foundation_(AppMessage)
import Data.Typeable(Typeable)
import Hap.Dictionary.Utils(showEF)
import Hap.Dictionary.DicTypes
-- import Hap.Dictionary.DicTypes as EDSL(HasDictionary)

------------------- DicEDSL -----------------------------
mkDic   :: (PersistEntity a, Typeable a, PersistEntityBackend a ~ SqlBackend)
        => AppMessage -> [DicField a] -> Dictionary a
mkDic m flds = Dictionary
    { dDisplayName  = m
    , dFields       = flds
    , dShowFunc     = showEF persistIdField
    }

fld :: (PersistEntity a, FieldForm a t)
    => EntityField a t -> DicField a
fld ef = DicField
    { dfEntityField = ef
    , dfSettings    = "" { fsLabel = hn }
    , dfShort       = Nothing
    , dfKind        = mempty
    -- , dfFieldForm   = ff
    }
  where
    hn = SomeMessage $ unHaskellName $ fieldHaskell $ persistFieldDef ef

label :: AppMessage -> DicField a -> DicField a
label mess f = f
    { dfSettings = (dfSettings f) { fsLabel = SomeMessage mess }
    , dfShort = Just $ fromMaybe mess $ dfShort f
    }
shortLabel :: AppMessage -> DicField a -> DicField a
shortLabel mess f = f { dfShort = Just mess }

hidden :: DicField a -> DicField a
hidden f = f { dfKind = Hidden }

readonly :: DicField a -> DicField a
readonly f = f { dfKind = ReadOnly <> dfKind f }

recShowField :: PersistEntity e => EntityField e t -> Dictionary e -> Dictionary e
recShowField ef dic = dic { dShowFunc = showEF ef }

showField :: PersistEntity e => DicField e -> Entity e -> Text
showField (DicField {..}) = showEF dfEntityField

