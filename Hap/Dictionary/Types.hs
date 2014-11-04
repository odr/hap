{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, ConstraintKinds
			, FlexibleInstances, LambdaCase, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses
			, FlexibleContexts, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hap.Dictionary.Types where

import Hap.Dictionary.Import
import Control.Monad(liftM2)
import Data.Monoid(Endo(..))
import qualified Data.Text as T
import Data.Typeable
import           Data.Map (Map)
import qualified Data.Map as M
import Data.Char(toLower)
import Safe(readMay)

import Hap.Dictionary.Hap
import Hap.Dictionary.Utils(getRoot)

-- import Debug.Trace(traceShowId)

data Dictionary master e = Dictionary
    { dDisplayName  :: SomeMessage master
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

class (RenderMessage m FormMessage) => FieldForm m e a where
    fieldAForm :: [e] -> FieldSettings m -> Maybe a -> AForm (HandlerT m IO) a

instance (PersistEntity a) => Default (Key a) where
    def = either (error . T.unpack . ("Can't create def for Key. " <>))
                id
                ( fromPersistValue
                    $ fromMaybe (error "Can't create def for Key. Can't convert from (-1)")
                      $ fromPathPiece "-1"
                )

instance (PersistEntity a, Default a) => Default (Entity a) where
    def = Entity def def

instance (PersistEntity e, Typeable e, HasDictionary m a, Typeable a, YesodHap m) 
		=> FieldForm m e (Key a) where
    fieldAForm _ fs ma
        | typeOf ([] :: [e]) == typeOf ([] :: [a])
            = debugFormInput "Key a" ma form
        | otherwise = areq (dicKeyField fs (mempty :: ([m], [a]))) fs ma
      where
        form = fmap intToKey (areq intField fs (fmap (read . T.unpack . toPathPiece) ma :: Maybe Integer))
          where
            intToKey val 
                | val == -1 = def 
                | otherwise = fromMaybe (error "Invalid Key") $ fromPathPiece $ T.pack $ show $ val

instance (PersistEntity e, HasDictionary m a, YesodHap m) 
		=> FieldForm m e (Maybe (Key a)) where
    fieldAForm _ fs ma = debugFormInput "Maybe (Key a)" ma $ aopt (dicKeyField fs (mempty :: ([m],[a]))) fs ma

dicKeyField :: (Typeable a, HasDictionary m a
                , YesodPersist m, PersistStore (YesodPersistBackend m), PersistQuery (YesodPersistBackend m)
                , Yesod m, RenderMessage m HapMessage) 
            => FieldSettings m -> ([m],[a]) -> Field (HandlerT m IO) (Key a)
dicKeyField fs (x :: ([m],[a])) = Field
    { fieldParse = \rawVals _ -> do
        $logDebug $ debugMess "dicKeyField fieldParse: {}" (Only $ Shown rawVals)
        let err = debugMess "Invalid rawVals in dicKeyField for Key of '{}'. rawVals = {}" (dicText, Shown rawVals)
        case rawVals of
            [a] | fromPathPiece a == Just (def :: Key a) 
                    -> return $ Right Nothing
            [a,_]   -> return $ Right $ (fromPathPiece a :: Maybe (Key a))
            _       -> $logError err >> return (Left $ SomeMessage err)
    , fieldView = fv
    , fieldEnctype = UrlEncoded
    }
  where
    dicText = T.pack $ show $ SomeDictionary x
    (dn, showFunc)  = case getDictionary :: Dictionary m a of
        (Dictionary {..}) -> (dDisplayName, dShowFunc)
    fv idAttr nameAttr otherAttrs eResult isReq = do
        $logDebug $ "fieldView: " <> T.pack (show eResult)
        case eResult of
            Left r  | T.null r  -> fvWidget def ""
                    | otherwise -> setMessage $ toHtml r
            Right k -> do
                $logDebug $ "lookup for key " <> toPathPiece k
                val <- do
                    mval <- liftHandlerT $ fmap (fmap $ showFunc . Entity k) $ runDB $ get k
                    $logDebug $ "get entity: " <> T.pack (show mval)
                    case mval of 
                        Nothing -> do
                            mrHap <- getMessageRender
                            mr <- getMessageRender
                            setMessage $ toHtml $ mrHap $ MsgNotFound (mr dn) (toPathPiece k)
                            return mempty
                        Just v -> return v
                fvWidget k val
      where
        fvWidget k val = do
            toWidget [cassius|
                    .inline-button
                        margin-bottom: 10px
                        padding-bottom: 4px
                    .req-aster
                        font-size: large
                        color: red
                |]
            cnt <- liftHandlerT $ runDB $ count ([] :: [Filter a])
            root <- getRoot
            let edR = editR root (SomeDictionary x) $ toPersistValue k
                lstR = listR root $ SomeDictionary x
            lstHead <- liftM2 (\mrHap mr -> mrHap $ MsgSelDictionary (mr dn) $ mr $ fsLabel fs) 
                            getMessageRender getMessageRender
            [whamlet|
                    $if isReq
                        <span .req-aster>*
                    <input id=#{idAttr <> "_id"} type=hidden value=#{toPathPiece k} _value=#{toPathPiece k}>
                    <input ##{idAttr} name=#{nameAttr} *{otherAttrs} type=text value=#{val} _value=#{val} readonly>
                    <button type=button .inline-button .ui-chooser-select-button 
                        onclick=showPager('true','#{lstHead}','#{lstR}',#{cnt},'#{toPathPiece k}','#{idAttr}')>v
                    <button type=button onclick=window.open('#{edR}') .inline-button .ui-chooser-detail-button>d
                |]

editR :: Text -> SomeDictionary m -> PersistValue -> Text
editR root sd k = root <> "/edit/" <> T.pack (show sd) <> "/" <> toPathPiece k

listR :: Text -> SomeDictionary m -> Text
listR root sd = root <> "/list/" <> T.pack (show sd)


dicFieldAForm :: PersistEntity e => DicField m e -> Entity e -> AForm (HandlerT m IO) (Entity e)
dicFieldAForm (DicField {..}) (ent :: Entity e) 
    = fieldLens dfEntityField (fieldAForm ([] :: [e]) dfSettings . Just) ent 

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
    = fmap ((fmap (Last . Just) &&& caseRes) *** Endo) $ dicFieldForm df $ getLast' le
  where
    caseRes = \case
        FormSuccess le' -> Last $ Just le'
        _               -> le

class 	(Default e, PersistEntity e, PersistEntityBackend e ~ YesodPersistBackend m
		, Typeable e, PersistField e
		, PathPiece (Key e), Show e
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

class (Yesod m, RenderMessage m FormMessage, RenderMessage m HapMessage
        , YesodPersist m, PersistStore (YesodPersistBackend m), PersistQuery (YesodPersistBackend m)) 
    => YesodHap m where