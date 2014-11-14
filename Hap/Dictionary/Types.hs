{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, ConstraintKinds
			, FlexibleInstances, LambdaCase, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses
			, FlexibleContexts, OverloadedStrings, RecordWildCards
            , FunctionalDependencies, DeriveFunctor  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hap.Dictionary.Types where

import Hap.Dictionary.Import
import Control.Monad(liftM2)
import qualified Control.Monad.Trans.State as State
import Data.Monoid(Endo(..))
import qualified Data.Text as T
import Data.Typeable
import           Data.Map (Map)
import qualified Data.Map as M
import Data.Char(toLower)

import Hap.Dictionary.Hap
import Hap.Dictionary.Utils(getRoot, showPersistField, getByEF, setByEF)

class   (Default e, PersistEntity e, PersistEntityBackend e ~ YesodPersistBackend m
        , Typeable e, PersistField e, PathPiece (Key e), Show e, Eq e
        ) 
        => HasDictionary m e | e -> m where
    getDictionary :: Dictionary m e

class   ( Yesod m, RenderMessage m FormMessage, RenderMessage m HapMessage
        , YesodPersist m, PersistStore (YesodPersistBackend m), PersistQuery (YesodPersistBackend m)
        )
        => YesodHap m where

-- class HasSubDic m p e | e -> m p where
--     getSubDic :: Dictionary m e -> SubDic m p e

class HasMapDict m where
	getMapDict :: Map String (SomeDictionary m)
    -- getDict :: String -> SomeDictionary m

data Layout t 
    = Horizontal [Layout t]
    | Vertical [Layout t]
    | Layout t
    deriving (Show, Functor)

transposeLayout :: Layout t -> Layout t 
transposeLayout (Horizontal xs) = Vertical $ map transposeLayout xs
transposeLayout (Vertical xs) = Horizontal $ map transposeLayout xs
transposeLayout x@(Layout _) = x

ignoreLayout :: Layout t -> [t]
ignoreLayout (Horizontal xs)    = concatMap ignoreLayout xs
ignoreLayout (Vertical xs)      = concatMap ignoreLayout xs
ignoreLayout (Layout x)         = [x]


data Dictionary m e = Dictionary
    { dDisplayName  :: SomeMessage m
    , dPrimary      :: DicField m e
    , dFields       :: Layout (DicField m e)
    , dShowFunc     :: Entity e -> Text
    -- , dSubDics      :: [SomeSubDic m e]
    }

data SomeDictionary m
    = forall a. (HasDictionary m a) => SomeDictionary { unSomeDictionary :: [a] }

instance Show (SomeDictionary m) where
    show (SomeDictionary (_:: [e])) = show $ typeRep (Proxy :: Proxy e)

instance Eq (SomeDictionary m) where
    (==) = (==) `on` show

instance Ord (SomeDictionary m) where
    compare = compare `on` show

instance HasMapDict m => Read (SomeDictionary m) where
    readsPrec _ = \s -> [(maybe (error "Can't parse Dictionary") id $ M.lookup (map toLower s) getMapDict, "")]

----- Fields ----    

data FieldKind = Hidden | ReadOnly | Editable deriving (Eq, Show, Read)

instance Monoid FieldKind where
    mempty = Editable
    mappend a b
        | Hidden `elem` [a,b]   = Hidden
        | ReadOnly `elem` [a,b] = ReadOnly
        | otherwise             = Editable

data Ref m e = forall r. HasDictionary m r => Ref [m] (EntityField r (Key e))

class ForeignKey a r e | a -> r e where
    filterFK    :: a -> Key e -> Filter r
    setFK       :: a -> Key e -> Entity r -> Entity r
instance (PersistEntity r, PersistField (Key e)) 
        => ForeignKey (EntityField r (Key e)) r e where
    filterFK ef key = ef ==. key
    setFK = setByEF
instance (PersistEntity r, PersistField (Key e)) 
        => ForeignKey (EntityField r (Maybe (Key e))) r e where
    filterFK ef key = ef ==. Just key
    setFK ef key = setByEF ef (Just key)

data DicFieldIndex m e 
    = forall t. (FieldForm m e t, FieldToText m t) => NormalField [m] (EntityField e t)
    | forall r a. (HasDictionary m r, ForeignKey a r e) => RefField [(m,r)] a Int

data DicField m e   = {- forall t. (FieldForm m e t, FieldToText m t) 
                    => -} DicField 
    { dfIndex       :: DicFieldIndex m e -- EntityField e t
    , dfSettings    :: FieldSettings m
    , dfShort       :: Maybe (SomeMessage m)
    , dfKind        :: FieldKind
    }

isNormal :: DicField m e -> Bool
isNormal (DicField {..}) = case dfIndex of 
    NormalField{..} -> True
    _  -> False

isRef :: DicField m e -> Bool
isRef (DicField {..}) = case dfIndex of 
    RefField{..} -> True
    _  -> False

class FieldToText m a where
    fieldToText :: a -> HandlerT m IO (Maybe Text)

instance (HasDictionary m a, YesodPersist m, PersistStore (YesodPersistBackend m)) 
        => FieldToText m (Key a) where
    fieldToText k = runDB $ fmap (fmap $ showFunc . Entity k) $ get k
      where
        showFunc  = case getDictionary :: Dictionary m a of
            (Dictionary {..}) -> dShowFunc

instance FieldToText m a => FieldToText m (Maybe a) where
    fieldToText = maybe (return Nothing) fieldToText

entityToTexts :: (HasDictionary m a, YesodHap m) => [m] -> Entity a -> HandlerT m IO [Maybe Text]
entityToTexts (_ :: [m]) (ent :: Entity a) = fmap ((Just (showPersistField $ entityKey ent):) . reverse)
                    $ State.execStateT  (mapM_ (\df -> fToT df ent) fields
                                        ) [] 
  where
    fields = case getDictionary :: Dictionary m a of
        (Dictionary {..}) -> ignoreLayout dFields
    fToT :: (PersistEntity a) => DicField m a -> Entity a -> State.StateT [Maybe Text] (HandlerT m IO) (Entity a)
    fToT (DicField{..}) ent = case dfIndex of
        NormalField _ ef -> fieldLens ef 
                                (\fld -> lift (fieldToText fld) >>= State.modify . (:) >> return fld) 
                                ent
        RefField (_::[(m,r)]) ef _ -> do
            let showFunc = case getDictionary :: Dictionary m r of 
                    (Dictionary {..}) -> dShowFunc
            t <- lift   $ fmap (T.intercalate "; " . map showFunc) 
                        $ runDB $ selectList [filterFK ef $ entityKey ent] []
            State.modify (Just t:)
            return ent

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

instance (PersistEntity e, Typeable e, HasDictionary m a, YesodHap m) 
		=> FieldForm m e (Key a) where
    fieldAForm _ fs ma = areq (dicKeyField fs (mempty :: [a])) fs ma

instance (PersistEntity e, HasDictionary m a, YesodHap m) 
		=> FieldForm m e (Maybe (Key a)) where
    fieldAForm _ fs ma = debugFormInput "Maybe (Key a)" ma $ aopt (dicKeyField fs (mempty :: [a])) fs ma

dicKeyField :: (HasDictionary m a
                , YesodPersist m, PersistStore (YesodPersistBackend m), PersistQuery (YesodPersistBackend m)
                , Yesod m, RenderMessage m HapMessage) 
            => FieldSettings m -> [a] -> Field (HandlerT m IO) (Key a)
dicKeyField (fs :: FieldSettings m) (x :: [a]) = Field
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
                    <input id=#{idAttr <> "_id"} name=#{nameAttr} type=hidden value=#{toPathPiece k} _value=#{toPathPiece k}>
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
    = case dfIndex of
        NormalField _ ef    -> fieldLens ef (fieldAForm ([] :: [e]) dfSettings . Just) ent 
        RefField    (_ :: [(m,r)]) ef _
                            -> undefined
            {-
            rents <- lift $ runDB $ selectList [ef ==. entityKey ent] []
            return ent
            -}

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

dictionaryForm :: HasDictionary m e
    => Maybe (Entity e) 
    -> MForm (HandlerT m IO) ((FormResult (Entity e), Entity e), [FieldView m] -> [FieldView m])
dictionaryForm me
    = fmap ((fmap getLast' *** getLast') *** appEndo) 
    $ foldM app ((FormSuccess e0, e0), mempty) $ ignoreLayout $ dFields getDictionary
  where
    e0 = Last me
    app acc@((_,le),_) df = fmap (acc <>) $ dicFieldForm' df le

dictionaryAForm :: HasDictionary m e => Maybe (Entity e) -> AForm (HandlerT m IO) (Entity e)
dictionaryAForm = formToAForm . fmap (fst *** ($ [])) . dictionaryForm

