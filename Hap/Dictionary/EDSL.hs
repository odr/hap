{-# LANGUAGE RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}
module Hap.Dictionary.EDSL
	( module Hap.Dictionary.EDSL
	, HasDictionary(..)
    , HasMapDict(..)
	, SomeDictionary
    , YesodHap
    , Layout (..)
    , FieldForm (..)
    , FieldToText (..)
	) where

import Hap.Dictionary.Import as Hap.Dictionary.EDSL
import qualified Control.Monad.Trans.State as S
import Data.List(find)
import qualified Data.Traversable as TR
import Data.Typeable(Typeable)
import Hap.Dictionary.Utils(showEF)
import Hap.Dictionary.Types
import Hap.Dictionary.FieldFormI as Hap.Dictionary.EDSL()
-- import Yesod

listFieldAForm :: (RenderMessage m FormMessage, RenderMessage m mess, Eq a) 
        => [(mess, a)] -> [e] -> FieldSettings m -> Maybe a -> AForm (HandlerT m IO) a
listFieldAForm xs _ fs ma = areq (selectFieldList xs) fs ma

listFieldAFormOpt :: (RenderMessage m FormMessage, RenderMessage m mess, Eq a) 
        => [(mess, a)] -> [e] -> FieldSettings m -> Maybe (Maybe a) -> AForm (HandlerT m IO) (Maybe a)
listFieldAFormOpt xs _ fs ma = aopt (selectFieldList xs) fs ma

listFieldToText :: (RenderMessage m mess, Eq a) 
        => [(mess, a)] -> a -> HandlerT m IO (Maybe Text)
listFieldToText xs x = getMessageRender >>= \mr -> return (fmap (mr . fst) $ find ((x==) . snd) xs)

------------------- DicEDSL -----------------------------
mkDic   :: (RenderMessage m mess, PersistEntity a, Typeable a, PersistEntityBackend a ~ SqlBackend)
        => mess -> {-DicField m a -> -}Layout (DicField m a) -> Dictionary m a
mkDic m flds = Dictionary
    { dDisplayName  = SomeMessage m
    , dFields       = S.evalState (TR.mapM (\df@(DicField{dfIndex}) -> 
                            case dfIndex of
                                RefField a b _ -> do
                                    n <- S.get
                                    S.put $ n + 1
                                    return df { dfIndex = RefField a b n }
                                _ -> return df
                        ) flds) (0 :: Int)
    , dShowFunc     = showEF persistIdField
    , dBeforeSave   = \_ -> return
    , dAfterSave    = const $ return ()
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

ref:: (HasDictionary m r, ForeignKey fk r a) => [(m,r)] -> fk -> DicField m a
ref (site::[(m,r)]) ef = DicField
    { dfIndex       = RefField site ef 0
    , dfSettings    = "" { fsLabel = dispName }
    , dfShort       = Nothing 
    , dfKind        = mempty
    } 
  where
    dispName = case getDictionary :: Dictionary m r of
        Dictionary {..} -> dDisplayName

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

{-
showField :: PersistEntity e => DicField m e -> Entity e -> Text
showField (DicField {..}) = case dfIndex of 
    NormalField _ ef -> showEF ef
    RefField _ (ef :: EntityField r (Key e)) _ -> undefined
-}

someDic :: HasDictionary master a => [a] -> SomeDictionary master
someDic xs = SomeDictionary xs

