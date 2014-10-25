{-# LANGUAGE RecordWildCards #-}
module Hap.Dictionary.EDSL
	( module Hap.Dictionary.EDSL
	, HasDictionary(..)
	, SomeDictionary(..)
	) where

import Import_
-- import Foundation_(AppMessage)
import Data.Typeable(Typeable)
import Hap.Dictionary.Utils(showEF)
import Hap.Dictionary.DicTypes
-- import Hap.Dictionary.DicTypes as EDSL(HasDictionary)

------------------- DicEDSL -----------------------------
mkDic   :: (RenderMessage (HandlerSite m) mess, PersistEntity a, Typeable a, PersistEntityBackend a ~ SqlBackend)
        => mess -> [DicField m a] -> Dictionary m a
mkDic m flds = Dictionary
    { dDisplayName  = SomeMessage m
    , dFields       = flds
    , dShowFunc     = showEF persistIdField
    }

fld :: (PersistEntity a, FieldForm m a t) => EntityField a t -> DicField m a
fld ef = DicField
    { dfEntityField = ef
    , dfSettings    = "" { fsLabel = hn }
    , dfShort       = Nothing
    , dfKind        = mempty
    -- , dfFieldForm   = ff
    }
  where
    hn = SomeMessage $ unHaskellName $ fieldHaskell $ persistFieldDef ef

label :: RenderMessage (HandlerSite m) mess => mess -> DicField m a -> DicField m a
label mess DicField {..} = DicField
    { dfSettings = dfSettings { fsLabel = SomeMessage mess }
    , dfShort = Just $ maybe (SomeMessage mess) SomeMessage dfShort
    , ..
    }

shortLabel :: RenderMessage (HandlerSite m) mess => mess -> DicField m a -> DicField m a
shortLabel mess DicField {..} = DicField { dfShort = Just $ SomeMessage mess, .. }

hidden :: DicField m a -> DicField m a
hidden f = f { dfKind = Hidden }

readonly :: DicField m a -> DicField m a
readonly f = f { dfKind = ReadOnly <> dfKind f }

recShowField :: PersistEntity e => EntityField e t -> Dictionary m e -> Dictionary m e
recShowField ef dic = dic { dShowFunc = showEF ef }

showField :: PersistEntity e => DicField m e -> Entity e -> Text
showField (DicField {..}) = showEF dfEntityField

