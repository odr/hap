{-# LANGUAGE ExistentialQuantification, RankNTypes, ScopedTypeVariables, FlexibleInstances, RecordWildCards, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hap.Dictionary.DicTypes where
import Import_
import Control.Monad(foldM)
import Data.Monoid
import Foundation_(App, AppMessage)
import Data.Typeable(Typeable, typeRep, Proxy(..))
import qualified Data.Text as T
--import Foundation()

instance (PersistEntity a) => Default (Key a) where
    def = either (error . T.unpack . ("Can't create def for Key. " <>))
                id
                ( fromPersistValue
                    $ fromMaybe (error "Can't create def for Key. Can't convert from (-1)")
                      $ fromPathPiece "-1"
                )

instance (PersistEntity a, Default a) => Default (Entity a) where
    def = Entity def def

class (Default e, PersistEntity e, PersistField e, Typeable e, PersistEntityBackend e ~ SqlBackend, PathPiece (Key e))
    => HasDictionary e where
    getDictionary :: Dictionary e

data Dictionary e = Dictionary
    { dDisplayName  :: AppMessage
    , dFields       :: [DicField e]
    , dShowFunc     :: Entity e -> Text
    }

data SomeDictionary
    = forall a. HasDictionary a
    => SomeDictionary { unSomeDictionary :: [a] }

instance Show SomeDictionary where
    show (SomeDictionary (_:: [e])) = show $ typeRep (Proxy :: Proxy e)

instance Eq SomeDictionary where
    (==) = (==) `on` show

instance Ord SomeDictionary where
    compare = compare `on` show

data FieldKind = Hidden | ReadOnly | Editable deriving (Eq, Show, Read)

instance Monoid FieldKind where
    mempty = Editable
    mappend a b
        | Hidden `elem` [a,b]   = Hidden
        | ReadOnly `elem` [a,b] = ReadOnly
        | otherwise             = Editable

data DicField e = forall t. FieldForm e t => DicField
    { dfEntityField :: EntityField e t
    , dfSettings    :: FieldSettings App
    , dfShort       :: Maybe AppMessage
    , dfKind        :: FieldKind
    }

getDBName :: (PersistEntity e) => DicField e -> Text
getDBName (DicField {..}) = unDBName $ fieldDB $ persistFieldDef dfEntityField

class (PersistEntity e) => FieldForm e a where
    fieldAForm
        :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m, MonadLogger m)
        => [e] -> FieldSettings (HandlerSite m) -> Maybe a -> AForm m a
    fieldForm
        :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m, MonadLogger m)
        => [e] -> FieldSettings (HandlerSite m) -> Maybe a -> MForm m (FormResult a, FieldView site)

instance (PersistEntity e, HasDictionary a) => FieldForm e (Key a) where
    fieldAForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (fromMaybe (error "Invalid Key") . fromPathPiece . T.pack . show)
            $ areq intField fs (fmap (read . T.unpack . toPathPiece) ma :: Maybe Integer)
        | otherwise = areq (dicKeyField ([] :: [a])) fs ma
    fieldForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (first $ fmap $ fromMaybe (error "Invalid Key") . fromPathPiece . T.pack . show)
            $ mreq intField fs (fmap (read . T.unpack . toPathPiece) ma :: Maybe Integer)
        | otherwise = mreq (dicKeyField ([] :: [a])) fs ma

instance (PersistEntity e, HasDictionary a) => FieldForm e (Maybe (Key a)) where
    fieldAForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (>>= fromPathPiece . T.pack . show)
            $ aopt intField fs (fmap (fmap (read . T.unpack . toPathPiece)) ma :: Maybe (Maybe Integer))
        | otherwise = aopt (dicKeyField ([] :: [a])) fs ma
    fieldForm _ fs ma
        | show (def :: Key e) == show (def :: Key a)
            = fmap (first $ fmap (>>= fromPathPiece . T.pack . show))
            $ mopt intField fs (fmap (fmap (read . T.unpack . toPathPiece)) ma :: Maybe (Maybe Integer))
        | otherwise = mopt (dicKeyField ([] :: [a])) fs ma

dicKeyField
    :: (HasDictionary a, RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m, MonadLogger m)
    => [a] -> Field m (Key a)
dicKeyField (x :: [a]) = Field
    { fieldParse = \rawVals _ ->
        let err = $logError $ "Invalid rawVals in dicKeyField for Key of '"
                            <> T.pack (show  $ SomeDictionary x)
                            <> "'. rawVals = [" <> T.intercalate ", " rawVals <> "]"
        in
        case rawVals of
            [a] -> return $ Right $ (fromPathPiece a :: Maybe (Key a))-- getKey a
            [] -> return $ Right Nothing
            _ -> err >> return (Left "More than one rawVal in dicKeyField")
    , fieldView = \idAttr nameAttr otherAttrs eResult _ -> do
        either
            (setMessage . toHtml)
            (\k -> [whamlet|
                    <span .ui-single-line>
                        <input id=#{idAttr} name=#{nameAttr} *{otherAttrs}>
                        <a href="" .ui-chooser-view-button>v
                        <a href=#{editR k} target=_blank .ui-chooser-detail-button>d
                |]
                -- @{EditR (SomeDictionary x) k}
            )
            eResult
    , fieldEnctype = UrlEncoded
    }
  where
    editR k = "/edit/" <> show (SomeDictionary x) <> "/" <> T.unpack (toPathPiece k)

dicFieldAForm :: (PersistEntity e, HandlerSite m ~ App, MonadHandler m, MonadLogger m)
     => DicField e -> Entity e -> AForm m (Entity e)
dicFieldAForm (DicField (ef :: EntityField e t) fs _ _) ent = fieldLens ef (fieldAForm ([]::[e]) fs . Just) ent

dicFieldForm :: (PersistEntity e, HandlerSite m ~ App, MonadHandler m, MonadLogger m)
    => DicField e -> Entity e -> MForm m (FormResult (Entity e), [FieldView App] -> [FieldView App])
dicFieldForm df = aFormToForm . dicFieldAForm df

getLast' :: (Default a) => Last a -> a
getLast' = fromMaybe def . getLast

dicFieldForm' :: (HasDictionary e, HandlerSite m ~ App, MonadHandler m, MonadLogger m)
    => DicField e -> Last (Entity e)
    -> MForm m ((FormResult (Last (Entity e)), Last (Entity e)), Endo [FieldView App])
dicFieldForm' df le
    = fmap ((fmap (Last . Just) &&& caseRes) *** Endo)
    $ dicFieldForm df $ getLast' le
  where
    caseRes = \case
        FormSuccess le' -> Last $ Just le'
        _               -> le

dictionaryForm :: (HasDictionary e, HandlerSite m ~ App, MonadHandler m, MonadLogger m)
    => Maybe (Entity e) -> MForm m ((FormResult (Entity e), Entity e), [FieldView App] -> [FieldView App])
dictionaryForm me
    = fmap ((fmap getLast' *** getLast') *** appEndo)
    $ foldM app ((FormSuccess e0, e0), mempty) $ dFields getDictionary
  where
    e0 = Last me
    app acc@((_,le),_) df = fmap (acc <>) $ dicFieldForm' df le

dictionaryAForm :: (HasDictionary e, HandlerSite m ~ App, MonadHandler m, MonadLogger m)
    => Maybe (Entity e) -> AForm m (Entity e)
dictionaryAForm = formToAForm . fmap (fst *** ($ [])) . dictionaryForm

