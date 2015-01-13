{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, ConstraintKinds
			, FlexibleInstances, LambdaCase, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses
			, FlexibleContexts, OverloadedStrings, RecordWildCards
            , FunctionalDependencies, DeriveFunctor, DeriveFoldable, DeriveTraversable
            , NoImplicitPrelude
            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hap.Dictionary.Types where

import Hap.Dictionary.Import
import GHC.Read(Read(..))
import Control.Lens
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Writer(WriterT(..))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Foldable as FD
import qualified Data.Traversable as TR

import Hap.Dictionary.Hap
import Hap.Dictionary.Utils(getRoot, showPersistField, setByEF, eqEF)

class   (Default e, PersistEntity e, PersistEntityBackend e ~ YesodPersistBackend m
        , Typeable e, PersistField e, PathPiece (Key e), Show e, Eq e
        ) 
        => HasDictionary m e | e -> m where
    getDictionary :: Dictionary m e

class   ( Yesod m, RenderMessage m FormMessage, RenderMessage m HapMessage
        , YesodPersist m, PersistStore (YesodPersistBackend m), PersistQuery (YesodPersistBackend m)
        )
        => YesodHap m where

class HasMapDict m where
    getMapDict :: Map String (SomeDictionary m)

data Layout t 
    = Horizontal [Layout t]
    | Vertical [Layout t]
    | Layout t
    deriving (Show, Functor, FD.Foldable, TR.Traversable) -- )

instance Applicative Layout where
    pure = Layout
    (Layout     f)  <*> x = fmap f x
    (Horizontal fs) <*> x = Horizontal $ map (<*> x) fs
    (Vertical   ts) <*> x = Vertical   $ map (<*> x) ts

instance Monad Layout where
    return = Layout
    (Horizontal ts) >>= f = Horizontal $ map (join . fmap f) ts
    (Vertical ts)   >>= f = Vertical   $ map (join . fmap f) ts
    Layout t        >>= f = f t

transposeLayout :: Layout t -> Layout t 
transposeLayout (Horizontal xs) = Vertical $ map transposeLayout xs
transposeLayout (Vertical xs) = Horizontal $ map transposeLayout xs
transposeLayout x@(Layout _) = x

ignoreLayout :: Layout t -> [t]
ignoreLayout (Horizontal xs)    = concatMap ignoreLayout xs
ignoreLayout (Vertical xs)      = concatMap ignoreLayout xs
ignoreLayout (Layout x)         = [x]

data ValidationLevel = ValidationError | ValidationWarning
    deriving (Eq, Ord, Show)

data IgnoreLevel = IgnoreNothing | IgnoreWarning | IgnoreAll   

type Validations = S.Set (ValidationLevel, Text)

filterValidation :: IgnoreLevel -> ValidationLevel -> Bool
filterValidation IgnoreNothing _ = True
filterValidation IgnoreWarning ValidationWarning = False
filterValidation IgnoreWarning _ = True
filterValidation IgnoreAll _ = False

filterValidations :: IgnoreLevel -> Validations -> Validations
filterValidations il = S.filter (filterValidation il . fst) 

data Dictionary m e = Dictionary
    { dDisplayName  :: SomeMessage m
    , dFields       :: Layout (DicField m e)
    , dShowFunc     :: Entity e -> Text
    , dBeforeSave   :: Maybe e -> e -> WriterT Validations (YesodDB m) e
    , dAfterSave    :: e -> YesodDB m ()
    }

instance Typeable e => Show (Dictionary m e) where
    show _ = show $ typeRep (Proxy :: Proxy e)

data SomeDictionary m
    = forall a. (HasDictionary m a) => SomeDictionary { unSomeDictionary :: [a] }

instance Show (SomeDictionary m) where
    show (SomeDictionary (_:: [e])) = show $ typeRep (Proxy :: Proxy e)

instance Eq (SomeDictionary m) where
    (==) = (==) `on` show

instance Ord (SomeDictionary m) where
    compare = compare `on` show

instance HasMapDict m => Read (SomeDictionary m) where
    readsPrec _ = \s -> [(maybe (error "Can't parse Dictionary") id $ M.lookup (toLower s) getMapDict, "")]

----- Fields ----    

data FieldKind = Hidden | ReadOnly | Editable deriving (Eq, Show, Read)

instance Monoid FieldKind where
    mempty = Editable
    mappend a b
        | Hidden `elem` [a,b]   = Hidden
        | ReadOnly `elem` [a,b] = ReadOnly
        | otherwise             = Editable

class ForeignKey a r e | a -> r e where
    filterFK    :: a -> Key e -> Filter r
    setFK       :: a -> Key e -> Entity r -> Entity r
    eqFK        :: forall t r'. PersistEntity r' => a -> EntityField r' t -> Bool

instance (PersistEntity r, PersistField (Key e)) => ForeignKey (EntityField r (Key e)) r e where
    filterFK ef key = ef ==. key
    setFK = setByEF
    eqFK = eqEF

instance (PersistEntity r, PersistField (Key e)) => ForeignKey (EntityField r (Maybe (Key e))) r e where
    filterFK ef key = ef ==. Just key
    setFK ef key = setByEF ef (Just key)
    eqFK = eqEF

data DicFieldIndex m e 
    = forall t. (FieldForm m e t, FieldToText m t) => NormalField [m] (EntityField e t)
    | forall r a. (HasDictionary m r, ForeignKey a r e) => RefField [(m,r)] a Int

data DicField m e   =  DicField 
    { dfIndex       :: DicFieldIndex m e 
    , dfSettings    :: FieldSettings m
    , dfShort       :: Maybe (SomeMessage m)
    , dfKind        :: FieldKind
    }

isNormal :: DicFieldIndex m e -> Bool
isNormal (NormalField {..}) =  True
isNormal _  = False

isRef :: DicFieldIndex m e -> Bool
isRef (RefField{..}) = True
isRef _  = False

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
entityToTexts (_ :: [m]) (ent0 :: Entity a) = fmap ((Just (showPersistField $ entityKey ent0):) . reverse)
                    $ State.execStateT  (mapM_ (\df -> fToT df ent0 >> return ()) fields
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

instance (Default t) => Default (FormResult t) where
    def = FormSuccess def

data EntityRef m e = forall r a. (HasDictionary m r, ForeignKey a r e) => EntityRef 
    { erForeignKey :: a 
    , erEntities   :: [EntityPlus m r]
    } 
instance Show (EntityRef m e) where
    show (EntityRef _ es) = "EntityRef: " ++ show es

data EntityPlus m e = EntityPlus
    { _epEntity :: Entity e
    , _epRefs   :: [EntityRef m e]
    }
makeLenses ''EntityPlus

instance (PersistEntity e, Show (Key e), Show e) => Show (EntityPlus m e) where
    show (EntityPlus e es) = "EntityPlus { epEntity = " ++ show e ++ ", " ++ show es ++ "}"

instance HasDictionary m e => Default (EntityPlus m e) where
    def = EntityPlus def 
        $ map (\(RefField (_::[(m,r)]) ef _) -> EntityRef ef []) 
        $ filter isRef 
        $ map dfIndex 
        $ ignoreLayout 
        $ dFields (getDictionary :: Dictionary m e)
