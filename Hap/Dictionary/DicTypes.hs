{-# LANGUAGE ExistentialQuantification, RankNTypes, ScopedTypeVariables, FlexibleInstances
            , RecordWildCards, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hap.Dictionary.DicTypes where
import Import_
import Control.Monad(foldM)
import Data.Char(toLower)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import Data.Typeable(Typeable, typeRep, Proxy(..))
import Safe(readMay)

import Hap.Dictionary.Utils

instance (PersistEntity a) => Default (Key a) where
    def = either (error . T.unpack . ("Can't create def for Key. " <>))
                id
                ( fromPersistValue
                    $ fromMaybe (error "Can't create def for Key. Can't convert from (-1)")
                      $ fromPathPiece "-1"
                )

instance (PersistEntity a, Default a) => Default (Entity a) where
    def = Entity def def

class HasMapDict m where
    getMapDict :: M.Map String (SomeDictionary m)

instance (HasMapDict m) => Read (SomeDictionary m) where
    readsPrec _ = \s -> [(maybe (error "Can't parse Dictionary") id $ M.lookup (map toLower s) getMapDict, "")]

instance (HasMapDict m) => PathPiece (SomeDictionary m) where
    toPathPiece = T.pack . show
    fromPathPiece = readMay . T.unpack

class   ( Default e, PersistEntity e, PersistField e, Typeable e
        , PersistEntityBackend e ~ SqlBackend, PathPiece (Key e)
        , MonadLogger m, MonadHandler m
        , Yesod (HandlerSite m), RenderMessage (HandlerSite m) FormMessage
        , HasMapDict m
        )
        => HasDictionary m e where
    getDictionary :: Dictionary m e

data Dictionary m e = Dictionary
    { dDisplayName  :: SomeMessage (HandlerSite m) -- forall mess. (RenderMessage (HandlerSite m) mess) => mess 
    , dFields       :: [DicField m e]
    , dShowFunc     :: Entity e -> Text
    }

data SomeDictionary m
    = forall a. HasDictionary m a
    => SomeDictionary { unSomeDictionary :: m [a] }

instance Show (SomeDictionary m) where
    show (SomeDictionary (_:: m [e])) = show $ typeRep (Proxy :: Proxy e)

instance Eq (SomeDictionary m) where
    (==) = (==) `on` show

instance Ord (SomeDictionary m) where
    compare = compare `on` show

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
    , dfSettings    :: FieldSettings (HandlerSite m)
    , dfShort       :: Maybe (SomeMessage (HandlerSite m))
    , dfKind        :: FieldKind
    }

getDBName :: (PersistEntity e) => DicField m e -> Text
getDBName (DicField {..}) = unDBName $ fieldDB $ persistFieldDef dfEntityField

class (HasDictionary m e) => FieldForm m e a where
    fieldAForm :: [e] -> FieldSettings (HandlerSite m) -> Maybe a -> AForm m a
    fieldForm :: [e] -> FieldSettings (HandlerSite m) -> Maybe a -> MForm m (FormResult a, FieldView (HandlerSite m))

instance (HasDictionary m e, HasDictionary m a) => FieldForm m e (Key a) where
    fieldAForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (fromMaybe (error "Invalid Key") . fromPathPiece . T.pack . show)
            $ areq intField fs (fmap (read . T.unpack . toPathPiece) ma :: Maybe Integer)
        | otherwise = areq (dicKeyField (return [] :: m [a])) fs ma
    fieldForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (first $ fmap $ fromMaybe (error "Invalid Key") . fromPathPiece . T.pack . show)
            $ mreq intField fs (fmap (read . T.unpack . toPathPiece) ma :: Maybe Integer)
        | otherwise = mreq (dicKeyField (return [] :: m [a])) fs ma

instance (HasDictionary m e, HasDictionary m a) => FieldForm m e (Maybe (Key a)) where
    fieldAForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (>>= fromPathPiece . T.pack . show)
            $ aopt intField fs (fmap (fmap (read . T.unpack . toPathPiece)) ma :: Maybe (Maybe Integer))
        | otherwise = aopt (dicKeyField (return [] :: m [a])) fs ma
    fieldForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (first $ fmap (>>= fromPathPiece . T.pack . show))
            $ mopt intField fs (fmap (fmap (read . T.unpack . toPathPiece)) ma :: Maybe (Maybe Integer))
        | otherwise = mopt (dicKeyField (return [] :: m [a])) fs ma

dicKeyField :: (HasDictionary m a) => m [a] -> Field m (Key a)
{- more common:
dicKeyField :: (HasDictionary m a, MonadLogger m1, Yesod (HandlerSite m1)) => m [a] -> Field m1 (Key a)
-}
dicKeyField (x :: m [a]) = Field
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
                path <- fmap (<> editR k) getRoot
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
    editR k = "/edit/" <> dicText <> "/" <> toPathPiece k
    dicText = T.pack $ show $ SomeDictionary x

dicFieldAForm :: HasDictionary m e => DicField m e -> Entity e -> AForm m (Entity e)
dicFieldAForm (DicField (ef :: EntityField e t) fs _ _) ent 
    = fieldLens ef (fieldAForm ([] :: [e]) fs . Just) ent 

dicFieldForm :: HasDictionary m e
    => DicField m e -> Entity e -> MForm m (FormResult (Entity e), [FieldView (HandlerSite m)] 
    -> [FieldView (HandlerSite m)])
dicFieldForm df = aFormToForm . dicFieldAForm df

getLast' :: (Default a) => Last a -> a
getLast' = fromMaybe def . getLast

dicFieldForm' :: HasDictionary m e
    => DicField m e -> Last (Entity e)
    -> MForm m ((FormResult (Last (Entity e)), Last (Entity e)), Endo [FieldView (HandlerSite m)])
dicFieldForm' df le
    = fmap ((fmap (Last . Just) &&& caseRes) *** Endo)
    $ dicFieldForm df $ getLast' le
  where
    caseRes = \case
        FormSuccess le' -> Last $ Just le'
        _               -> le

dictionaryForm :: HasDictionary m e
    => Maybe (Entity e) -> MForm m ((FormResult (Entity e), Entity e), [FieldView (HandlerSite m)] 
    -> [FieldView (HandlerSite m)])
dictionaryForm me
    = fmap ((fmap getLast' *** getLast') *** appEndo)
    $ foldM app ((FormSuccess e0, e0), mempty) $ dFields getDictionary
  where
    e0 = Last me
    app acc@((_,le),_) df = fmap (acc <>) $ dicFieldForm' df le

dictionaryAForm :: HasDictionary m e => Maybe (Entity e) -> AForm m (Entity e)
dictionaryAForm = formToAForm . fmap (fst *** ($ [])) . dictionaryForm

