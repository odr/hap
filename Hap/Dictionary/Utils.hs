{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings, NoImplicitPrelude #-}
module Hap.Dictionary.Utils where

import Hap.Dictionary.Import
import qualified Data.Map as M
import qualified Data.Text as T

showPersistValue :: PersistValue -> Text
showPersistValue (PersistText v)              = v
showPersistValue (PersistByteString _)        = "[binary]"
showPersistValue (PersistInt64 v)             = T.pack $ show v
showPersistValue (PersistDouble v)            = T.pack $ show v
showPersistValue (PersistRational v)          = T.pack $ show v
showPersistValue (PersistBool b)              = if b then "+" else "-"
showPersistValue (PersistDay v)               = T.pack $ formatTime defaultTimeLocale "%x" v
showPersistValue (PersistTimeOfDay v)         = T.pack $ formatTime defaultTimeLocale "%X" v
showPersistValue (PersistUTCTime v)           = T.pack $ formatTime defaultTimeLocale "%x %X" v
showPersistValue PersistNull                  = mempty
showPersistValue (PersistList xs)             = T.unlines $ map showPersistValue xs
showPersistValue (PersistMap xs)              = T.unlines $ map (\(a,b) -> a <> ": " <> showPersistValue b) xs
showPersistValue (PersistObjectId _)          = "[object]"
showPersistValue (PersistDbSpecific _)        = "[binary db-specific]"

showPersistField :: (PersistField a) => a -> Text
showPersistField = showPersistValue . toPersistValue

getByEF :: PersistEntity e => EntityField e t -> Entity e -> t
getByEF ef = getConst . fieldLens ef Const

setByEF :: PersistEntity e => EntityField e t -> t -> Entity e -> Entity e
setByEF ef val = runIdentity . fieldLens ef (Identity . const val)

eqEF :: (PersistEntity e1, PersistEntity e2) => EntityField e1 t1 -> EntityField e2 t2 -> Bool
eqEF (a :: EntityField e1 t1) (b :: EntityField e2 t2) 
    =  on (==) entityDB (entityDef ([]::[e1])) (entityDef ([]::[e2]))
    && on (==) fieldDB (persistFieldDef a) (persistFieldDef b)

entityFieldToPersist :: PersistEntity e => EntityField e t -> Entity e -> SomePersistField
entityFieldToPersist ef (Entity key (ent :: e))
    | persistFieldDef ef == persistFieldDef (persistIdField :: EntityField e (Key e))
        = SomePersistField key
    | otherwise = maybe (error $ unlines $
                                [ "Panic! Can't find entity field in function entityFieldToPersist"
                                , "persistFieldDef ef = " ++ show (persistFieldDef ef)
                                , "entityFields $ entityDef [ent] = ["
                                ]
                                ++ map show (entityFields $ entityDef [ent])
                                ++ ["]"]
            ) id
    $ lookup (persistFieldDef ef)
    $ zip (entityFields $ entityDef [ent]) $ toPersistFields ent

showEF :: PersistEntity e => EntityField e t -> Entity e -> Text
showEF ef = showPersistField . entityFieldToPersist ef

sortByPattern :: Ord a => (b -> a) -> (c -> a) -> [b] -> [c] -> [c]
sortByPattern f g ps = sortBy (comparing $ flip M.lookup m . g)
  where
    m = M.fromList $ zip (map f ps) [(1::Int)..]

getRoot :: (MonadHandler f, Yesod (HandlerSite f)) => f Text
getRoot = case approot of
    ApprootMaster f -> fmap f getYesod
    ApprootStatic t -> return t
    ApprootRelative -> return ".."
    ApprootRequest f -> liftM2 f getYesod (fmap reqWaiRequest getRequest)

widgetToHtml :: (Yesod site) => WidgetT site IO () -> HandlerT site IO Html
widgetToHtml = fmap pageBody . widgetToPageContent >=> withUrlRenderer

setMessageWidget :: (Yesod site) => WidgetT site IO () -> HandlerT site IO ()
setMessageWidget = widgetToHtml >=> setMessage
