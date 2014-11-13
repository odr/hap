{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Hap.Dictionary.EDSL
	( module Hap.Dictionary.EDSL
	, HasDictionary(..)
    , HasMapDict(..)
	, SomeDictionary
    , YesodHap
	) where

import Hap.Dictionary.Import as Hap.Dictionary.EDSL
import Data.Typeable(Typeable)
import Hap.Dictionary.Utils(showEF)
import Hap.Dictionary.Types
import Hap.Dictionary.FieldFormI as Hap.Dictionary.EDSL()
import Yesod

------------------- DicEDSL -----------------------------
mkDic   :: (RenderMessage m mess, PersistEntity a, Typeable a, PersistEntityBackend a ~ SqlBackend)
        => mess -> DicField m a -> [DicField m a] -> Dictionary m a
mkDic m pk flds = Dictionary
    { dDisplayName  = SomeMessage m
    , dPrimary      = pk
    , dFields       = Vertical $ map Layout flds
    , dShowFunc     = showEF persistIdField
    -- , dSubDics      = []
    }

fld :: (PersistEntity a, FieldForm m a t, FieldToText m t) => EntityField a t -> DicField m a
fld ef = DicField
    { dfIndex       = NormalField ([]::[m]) ef
    , dfSettings    = "" { fsLabel = hn }
    , dfShort       = Nothing
    , dfKind        = mempty
    -- , dfFieldForm   = ff
    }
  where
    hn = SomeMessage $ unHaskellName $ fieldHaskell $ persistFieldDef ef

label :: RenderMessage m mess => mess -> DicField m a -> DicField m a
label mess DicField {..} = DicField
    { dfSettings = dfSettings { fsLabel = SomeMessage mess }
    , dfShort = Just $ maybe (SomeMessage mess) SomeMessage dfShort
    , ..
    }

shortLabel :: RenderMessage m mess => mess -> DicField m a -> DicField m a
shortLabel mess DicField {..} = DicField { dfShort = Just $ SomeMessage mess, .. }

hidden :: DicField m a -> DicField m a
hidden f = f { dfKind = Hidden }

readonly :: DicField m a -> DicField m a
readonly f = f { dfKind = ReadOnly <> dfKind f }

recShowField :: PersistEntity e => EntityField e t -> Dictionary m e -> Dictionary m e
recShowField ef dic = dic { dShowFunc = showEF ef }

showField :: PersistEntity e => DicField m e -> Entity e -> Text
showField (DicField {..}) = case dfIndex of 
    NormalField _ ef -> showEF ef
    RefField _ (ef :: EntityField r (Key e)) -> undefined

someDic :: HasDictionary master a => [a] -> SomeDictionary master
someDic xs = SomeDictionary xs

