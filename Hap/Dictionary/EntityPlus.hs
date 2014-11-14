{-# LANGUAGE ExistentialQuantification, RecordWildCards, ScopedTypeVariables #-}
module Hap.Dictionary.EntityPlus where

import Hap.Dictionary.Import 

import Control.Monad(when)
import Data.List((\\), find)

import Hap.Dictionary.Types
import Hap.Dictionary.Utils(setByEF)

data EntityPlus m e = EntityPlus
    { epEntity  :: Entity e
    , epRefs    :: [EntityRef m e]
    }

data EntityRef m e = forall r. HasDictionary m r => EntityRef (EntityField r (Key e)) [EntityPlus m r]

getEntityPlus :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Dictionary m e -> Key e -> YesodDB m (EntityPlus m e)
getEntityPlus dic key = 
    get key >>= maybe notFound (\ent -> EntityPlus (Entity key ent) <$> getRefsEP dic key)

getRefsEP :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Dictionary m e -> Key e -> YesodDB m [EntityRef m e]
getRefsEP (Dictionary{..} :: Dictionary m e) key = reverse <$> foldM getEP [] (map dfIndex $ ignoreLayout dFields)
  where
    getEP xs (NormalField{..}) = return xs
    getEP xs (RefField _ (ef :: EntityField r (Key e))) 
        = (\eps -> EntityRef ef eps : xs)
        <$> getFilteredEPs (getDictionary :: Dictionary m r) [ef ==. key]

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
            rs <- putRefs k epRefs
            return ep { epRefs = rs }
        )
  where
    k = entityKey epEntity
    v = entityVal epEntity
    hardIns = do
        key <- insert v
        rs <- insertRefs key epRefs
        return $ EntityPlus (Entity key v) rs

insertRefs :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Key e -> [EntityRef m e] -> YesodDB m [EntityRef m e]
insertRefs key = mapM (insertRef key)

insertRef :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Key e -> EntityRef m e -> YesodDB m (EntityRef m e)
insertRef key (EntityRef (ef :: EntityField r (Key e)) (eps :: [EntityPlus m r])) 
    = EntityRef ef <$> 
        mapM (\ep@(EntityPlus{..}) -> putEntityPlus Nothing ep { epEntity = setByEF ef key $ epEntity }) eps

putRefs :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Key e -> [EntityRef m e] -> YesodDB m [EntityRef m e]
putRefs key = mapM (putRef key)

putRef :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => Key e -> EntityRef m e -> YesodDB m (EntityRef m e)
putRef key (EntityRef (ef :: EntityField r (Key e)) (eps :: [EntityPlus m r])) = do
    ents' <- selectList [ef ==. key] []
    delEntities $ map entityKey ents' \\ map (entityKey . epEntity) eps
    EntityRef ef <$> 
        mapM    (\ep -> putEntityPlus 
                            (entityVal <$> find ((== entityKey (epEntity ep)) . entityKey) ents') 
                            ep { epEntity = setByEF ef key $ epEntity ep}
                ) eps

delEntities :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) => [Key e] -> YesodDB m ()
-- TODO: optionally delete cascade
delEntities keys = deleteWhere $ map (persistIdField ==.) keys
