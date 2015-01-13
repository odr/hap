{-# LANGUAGE ExistentialQuantification, RecordWildCards, ScopedTypeVariables, NoImplicitPrelude #-}
module Hap.Dictionary.EntityPlus where

import Hap.Dictionary.Import 

import Control.Monad.Trans.Writer(WriterT(..))
import Control.Monad.Trans.Except(ExceptT(..), throwE)
import qualified Data.List as L
import qualified Data.Text as T

import Hap.Dictionary.Types

-- getByFK :: (ForeignKey a r e) => a -> [EntityRef m e] -> [EntityPlus m r]
-- ngetByFK fk = concatMap erEntities . filter (eqFK fk . erForeignKey)

_entityKey :: Functor f => (Key e -> f (Key e)) -> Entity e -> f (Entity e)
_entityKey g e = fmap (\k -> e { entityKey = k }) $ g $ entityKey e

_entityVal :: Functor f => (e -> f e) -> Entity e -> f (Entity e)
_entityVal g e = fmap (\v -> e { entityVal = v }) $ g $ entityVal e

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

-- validateEntityPlus
type ExYDB m = ExceptT Validations (YesodDB m)

putEntityPlus :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) 
        => IgnoreLevel -> Maybe e -> EntityPlus m e -> ExYDB m (EntityPlus m e)
putEntityPlus il mbold (ep :: EntityPlus m e)
    | k == def  = ins Nothing
    | otherwise = lift (maybe (get k) (return . Just) mbold) >>= ins
  where
    dic = getDictionary :: Dictionary m e
    k = entityKey $ _epEntity ep
    v = entityVal $ _epEntity ep 
    ins mbep = do
        (v', valids) <- lift $ runWriterT $ dBeforeSave dic mbep v
        let valids' = filterValidations il valids
        if valids' == mempty 
            then maybe
                (do key <- lift $ insert v'
                    rs <- insertRefs key $ _epRefs ep
                    return $ EntityPlus (Entity key v') rs
                )
                (\epv -> do
                    when (epv /= v') $ lift $ replace k v'
                    rs <- putRefs k $ _epRefs ep
                    return ep { _epRefs = rs }
                )
                mbep
            else
                throwE valids'
      where
        insertRefs key = mapM (insertRef key)
        insertRef key (EntityRef ef (eps :: [EntityPlus m r])) 
            = EntityRef ef <$> 
                mapM (\ep' -> putEntityPlus il Nothing ep' { _epEntity = setFK ef key $ _epEntity ep' }) eps

        putRefs key = mapM putRef
          where
            putRef (EntityRef ef (eps :: [EntityPlus m r])) = do
                ents' <- lift$ selectList [filterFK ef key] []
                lift $ lift $ $logDebug $ debugMess "Dictionary {}. Delete keys: {}" 
                    ( Shown (getDictionary :: Dictionary m e)
                    , Shown $ map toPathPiece $ map entityKey ents' L.\\ map (entityKey . _epEntity) eps
                    )
                lift $ delEntities $ map entityKey ents' L.\\ map (entityKey . _epEntity) eps
                EntityRef ef <$> 
                    mapM    (\ep' -> putEntityPlus il
                                        (entityVal <$> find ((== entityKey (_epEntity ep')) . entityKey) ents') 
                                        ep' { _epEntity = setFK ef key $ _epEntity ep'}
                            ) eps

delEntities :: (HasDictionary m e, PersistQuery (YesodPersistBackend m)) => [Key e] -> YesodDB m ()
-- TODO: optionally delete cascade
delEntities ks 
    | null ks = return ()
    | otherwise = deleteWhere $ map (persistIdField ==.) ks
