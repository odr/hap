{-# LANGUAGE ExistentialQuantification, RecordWildCards, ScopedTypeVariables #-}
module Hap.Dictionary.EntityPlus where

import Hap.Dictionary.Import 

import Control.Monad(when)
import Data.List((\\), find)
import qualified Data.Text as T
import Control.Lens

import Hap.Dictionary.Types
import Hap.Dictionary.Utils(setByEF)

data EntityRef m e = forall r a. (HasDictionary m r, ForeignKey a r e) => EntityRef 
    { erForeignKey :: a 
    , erEntities   :: [EntityPlus m r]
    } 
instance Show (EntityRef m e) where
    show (EntityRef _ es) = "EntityRef: " ++ show es

data EntityPlus m e = EntityPlus
    { _epEntity  :: Entity e
    , _epRefs    :: [EntityRef m e]
    }
makeLenses ''EntityPlus

_entityKey :: Functor f => (Key e -> f (Key e)) -> Entity e -> f (Entity e)
_entityKey g e = fmap (\k -> e { entityKey = k }) $ g $ entityKey e

_entityVal :: Functor f => (e -> f e) -> Entity e -> f (Entity e)
_entityVal g e = fmap (\v -> e { entityVal = v }) $ g $ entityVal e

-- makeLenses ''EntityRef
instance (PersistEntity e, Show (Key e), Show e) => Show (EntityPlus m e) where
    show (EntityPlus e es) = "EntityPlus { epEntity = " ++ show e ++ ", " ++ show es ++ "}"


instance HasDictionary m e => Default (EntityPlus m e) where
    def = EntityPlus def 
        $ map (\(RefField (_::[(m,r)]) ef _) -> EntityRef ef []) 
        $ filter isRef 
        $ map dfIndex 
        $ ignoreLayout 
        $ dFields (getDictionary :: Dictionary m e)

getEntityPlus :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Dictionary m e -> Key e -> YesodDB m (EntityPlus m e)
getEntityPlus dic key = do
    lift $ $logDebug $ "getEntityPlus!! Dic = " <> T.pack (show dic) <> "; Key = " <> toPathPiece key
    get key >>= maybe notFound (\ent -> EntityPlus (Entity key ent) <$> getRefsEP dic key)

getRefsEP :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Dictionary m e -> Key e -> YesodDB m [EntityRef m e]
getRefsEP (Dictionary{..} :: Dictionary m e) key = reverse <$> foldM getEP [] (map dfIndex $ ignoreLayout dFields)
  where
    getEP xs (NormalField{..}) = return xs
    getEP xs (RefField (_::[(m,r)]) ef _)
        = (\eps -> EntityRef ef eps : xs)
        <$> getFilteredEPs (getDictionary :: Dictionary m r) [filterFK ef key]

getFilteredEPs :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Dictionary m e -> [Filter e] -> YesodDB m [EntityPlus m e]
getFilteredEPs dic fs 
    = selectList fs [] >>= mapM (\ent -> EntityPlus ent <$> getRefsEP dic (entityKey ent)) 

putEntityPlus :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Maybe e -> EntityPlus m e -> YesodDB m (EntityPlus m e)
putEntityPlus mbold ep@(EntityPlus{..}) 
    | k == def  = hardIns
    | otherwise = maybe (get k) (return . Just) mbold >>= maybe hardIns
        (\epv -> do
            when (epv /= v) $ replace k v
            rs <- putRefs k _epRefs
            return ep { _epRefs = rs }
        )
  where
    k = entityKey _epEntity
    v = entityVal _epEntity
    hardIns = do
        key <- insert v
        rs <- insertRefs key _epRefs
        return $ EntityPlus (Entity key v) rs

insertRefs :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Key e -> [EntityRef m e] -> YesodDB m [EntityRef m e]
insertRefs key = mapM (insertRef key)

insertRef :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Key e -> EntityRef m e -> YesodDB m (EntityRef m e)
insertRef key (EntityRef ef (eps :: [EntityPlus m r])) 
    = EntityRef ef <$> 
        mapM (\ep@(EntityPlus{..}) -> putEntityPlus Nothing ep { _epEntity = setFK ef key $ _epEntity }) eps

putRefs :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Key e -> [EntityRef m e] -> YesodDB m [EntityRef m e]
putRefs key = mapM (putRef key)

putRef :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Key e -> EntityRef m e -> YesodDB m (EntityRef m e)
putRef (key :: Key e) (EntityRef ef (eps :: [EntityPlus m r])) = do
    ents' <- selectList [filterFK ef key] []
    lift $ $logDebug $ debugMess "Dictionary {}. Delete keys: {}" 
        ( Shown (getDictionary :: Dictionary m e)
        , Shown $ map toPathPiece $ map entityKey ents' \\ map (entityKey . _epEntity) eps
        )
    delEntities $ map entityKey ents' \\ map (entityKey . _epEntity) eps
    EntityRef ef <$> 
        mapM    (\ep -> putEntityPlus 
                            (entityVal <$> find ((== entityKey (_epEntity ep)) . entityKey) ents') 
                            ep { _epEntity = setFK ef key $ _epEntity ep}
                ) eps

delEntities :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) => [Key e] -> YesodDB m ()
-- TODO: optionally delete cascade
delEntities keys 
    | null keys = return ()
    | otherwise = deleteWhere $ map (persistIdField ==.) keys
