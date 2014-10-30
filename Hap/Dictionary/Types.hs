{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, ConstraintKinds
			, FlexibleInstances, LambdaCase, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses
			, FlexibleContexts, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hap.Dictionary.Types where

import Import_
-- import Control.Monad.Trans.Resource(MonadResourceBase)
-- import Data.Monoid(Monoid(..), (<>), 
import Data.Monoid(Last(..), Endo(..))
-- import           Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
-- import Data.Function(on)
import           Data.Map (Map)
import qualified Data.Map as M
import Data.Char(toLower)
import Safe(readMay)
-- import Data.Default.Generics(Default(..))
-- import Data.Maybe(fromMaybe)
-- import Control.Arrow(first, (***), (&&&))
-- import Control.Monad(foldM, (>=>))

import Hap.Dictionary.Hap
import Hap.Dictionary.Utils(getRoot)


data Dictionary master e = Dictionary
    { dDisplayName  :: SomeMessage master -- forall mess. (RenderMessage (HandlerSite m) mess) => mess 
    , dFields       :: [DicField master e]
    , dShowFunc     :: Entity e -> Text
    } -- deriving Typeable

class HasMapDict master where
	getMapDict :: Map String (SomeDictionary master)

data SomeDictionary master
    = forall a. (HasDictionary master a) => SomeDictionary { unSomeDictionary :: ([master], [a]) }

instance Show (SomeDictionary master) where
    show (SomeDictionary (_:: ([m],[e]))) = show $ typeRep (Proxy :: Proxy e)

instance Eq (SomeDictionary master) where
    (==) = (==) `on` show

instance Ord (SomeDictionary master) where
    compare = compare `on` show

instance HasMapDict master => Read (SomeDictionary master) where
    readsPrec _ = \s -> [(maybe (error "Can't parse Dictionary") id $ M.lookup (map toLower s) getMapDict, "")]

instance HasMapDict master => PathPiece (SomeDictionary master) where
    toPathPiece = T.pack . show
    fromPathPiece = readMay . T.unpack

{-
instance (PersistEntity a) => PathPiece (Key a) where
	toPathPiece = toPathPiece . toPersistValue
	fromPathPiece = fromPathPiece >=> either (const Nothing) Just . fromPersistValue
-}

data FieldKind = Hidden | ReadOnly | Editable deriving (Eq, Show, Read)

instance Monoid FieldKind where
    mempty = Editable
    mappend a b
        | Hidden `elem` [a,b]   = Hidden
        | ReadOnly `elem` [a,b] = ReadOnly
        | otherwise             = Editable

data DicField m e   = forall t. (FieldForm m e t) 
                    => DicField 
    { dfEntityField :: EntityField e t
    , dfSettings    :: FieldSettings m
    , dfShort       :: Maybe (SomeMessage m)
    , dfKind        :: FieldKind
    }


getDBName :: (PersistEntity e) => DicField m e -> Text
getDBName (DicField {..}) = unDBName $ fieldDB $ persistFieldDef dfEntityField
-- type HandlerMaster m = HandlerT Hap (HandlerT m IO)

class (RenderMessage m FormMessage) => FieldForm m e a where
    fieldAForm :: [e] -> FieldSettings m -> Maybe a -> AForm (HandlerT m IO) a
    fieldForm  :: [e] -> FieldSettings m -> Maybe a -> MForm (HandlerT m IO) (FormResult a, FieldView m)

{-
instance (RenderMessage m FormMessage) => FieldForm m e Text where
    fieldAForm _ = areq textField
    fieldForm  _ = mreq textField

instance (RenderMessage m FormMessage) => FieldForm m e (Maybe Text) where
    fieldAForm _ = aopt textField
    fieldForm  _ = mopt textField
-}
instance (PersistEntity a) => Default (Key a) where
    def = either (error . T.unpack . ("Can't create def for Key. " <>))
                id
                ( fromPersistValue
                    $ fromMaybe (error "Can't create def for Key. Can't convert from (-1)")
                      $ fromPathPiece "-1"
                )

instance (PersistEntity a, Default a) => Default (Entity a) where
    def = Entity def def

instance (PersistEntity e, HasDictionary m a, Typeable a, Yesod m, RenderMessage m FormMessage) 
		=> FieldForm m e (Key a) where
    fieldAForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (fromMaybe (error "Invalid Key") . fromPathPiece . T.pack . show)
            $ areq intField fs (fmap (read . T.unpack . toPathPiece) ma :: Maybe Integer)
        | otherwise = areq (dicKeyField (mempty :: ([m], [a]))) fs ma
    fieldForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (first $ fmap $ fromMaybe (error "Invalid Key") . fromPathPiece . T.pack . show)
            $ mreq intField fs (fmap (read . T.unpack . toPathPiece) ma :: Maybe Integer)
        | otherwise = mreq (dicKeyField (mempty :: ([m], [a]))) fs ma

instance (PersistEntity e, Typeable a, Yesod m, HasDictionary m a, RenderMessage m FormMessage) 
		=> FieldForm m e (Maybe (Key a)) where
    fieldAForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (>>= fromPathPiece . T.pack . show)
            $ aopt intField fs (fmap (fmap (read . T.unpack . toPathPiece)) ma :: Maybe (Maybe Integer))
        | otherwise = aopt (dicKeyField (mempty :: ([m],[a]))) fs ma
    fieldForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (first $ fmap (>>= fromPathPiece . T.pack . show))
            $ mopt intField fs (fmap (fmap (read . T.unpack . toPathPiece)) ma :: Maybe (Maybe Integer))
        | otherwise = mopt (dicKeyField (mempty :: ([m],[a]))) fs ma

dicKeyField :: (Typeable a, Yesod m, HasDictionary m a) => ([m],[a]) -> Field (HandlerT m IO) (Key a)
dicKeyField (x :: ([m],[a])) = Field
    { fieldParse = \rawVals _ ->
        let err = $logError $ "Invalid rawVals in dicKeyField for Key of '"
                            <> dicText
                            <> "'. rawVals = [" <> T.intercalate ", " rawVals <> "]"
        in
        case rawVals of
            [a] -> return $ Right $ (fromPathPiece a :: Maybe (Key a))
            [] -> return $ Right Nothing
            _ -> err >> return (Left "More than one rawVal in dicKeyField")
    , fieldView = \idAttr nameAttr otherAttrs eResult _ -> do
        either        
            (setMessage . toHtml)
            (\k -> do
                path <- fmap (\r -> editR r (SomeDictionary x) (toPersistValue k)) getRoot
                [whamlet|
                        <span .ui-single-line>
                            <input id=#{idAttr} name=#{nameAttr} *{otherAttrs}>
                            <a href="" .ui-chooser-view-button>v
                            <a href=#{path} target=_blank .ui-chooser-detail-button>d
                    |]
            )
            eResult
    , fieldEnctype = UrlEncoded
    }
  where
    dicText = T.pack $ show $ SomeDictionary x

editR :: Text -> SomeDictionary m -> PersistValue -> Text
editR root sd k = root <> "/edit/" <> T.pack (show sd) <> "/" <> toPathPiece k

listR :: Text -> SomeDictionary m -> Text
listR root sd = root <> "/list/" <> T.pack (show sd)


dicFieldAForm :: PersistEntity e => DicField m e -> Entity e -> AForm (HandlerT m IO) (Entity e)
dicFieldAForm (DicField (ef :: EntityField e t) fs _ _) ent 
    = fieldLens ef (fieldAForm ([] :: [e]) fs . Just) ent 

dicFieldForm :: PersistEntity e
    => DicField m e -> Entity e 
    -> MForm (HandlerT m IO) (FormResult (Entity e), [FieldView m] -> [FieldView m])
dicFieldForm df = aFormToForm . dicFieldAForm df

getLast' :: (Default a) => Last a -> a
getLast' = fromMaybe def . getLast

dicFieldForm' :: (Default e, PersistEntity e) 
    => DicField m e -> Last (Entity e)
    -> MForm (HandlerT m IO) ((FormResult (Last (Entity e)), Last (Entity e)), Endo [FieldView m])
dicFieldForm' df le
    = fmap ((fmap (Last . Just) &&& caseRes) *** Endo)
    $ dicFieldForm df $ getLast' le
  where
    caseRes = \case
        FormSuccess le' -> Last $ Just le'
        _               -> le


class 	(Default e, PersistEntity e, PersistEntityBackend e ~ YesodPersistBackend m
		, YesodPersist m, PersistQuery (YesodPersistBackend m), Yesod m
		, Typeable e, RenderMessage m HapMessage, PersistField e
		, PathPiece (Key e)
		) 
		=> HasDictionary m e where
	getDictionary :: Dictionary m e


dictionaryForm :: HasDictionary m e
    => Maybe (Entity e) 
    -> MForm (HandlerT m IO) ((FormResult (Entity e), Entity e), [FieldView m] -> [FieldView m])
dictionaryForm me
    = fmap ((fmap getLast' *** getLast') *** appEndo)
    $ foldM app ((FormSuccess e0, e0), mempty) $ dFields getDictionary
  where
    e0 = Last me
    app acc@((_,le),_) df = fmap (acc <>) $ dicFieldForm' df le

dictionaryAForm :: HasDictionary m e => Maybe (Entity e) -> AForm (HandlerT m IO) (Entity e)
dictionaryAForm = formToAForm . fmap (fst *** ($ [])) . dictionaryForm

{-
data HapRoute 
	= forall m. (HasMapDict m) 
	=> HapListR (SomeDictionary m)
	|  HapEditR (SomeDictionary m) (Key e)

render :: HapRoute -> [(Text, Text)] -> Text
render 
-}