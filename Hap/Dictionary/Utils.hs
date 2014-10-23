{-# LANGUAGE ScopedTypeVariables #-}
module Hap.Dictionary.Utils where

import Import_
import qualified Data.Text as T
import qualified Data.Map as M
import System.Locale(defaultTimeLocale)
import Data.Time(formatTime)
import Data.Ord(comparing)
import Data.List(sortBy)

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
-- showPersistValue (PersistZonedTime (ZT v))    = T.pack $ formatTime defaultTimeLocale "%c" v
showPersistValue PersistNull                  = mempty
showPersistValue (PersistList xs)             = T.unlines $ map showPersistValue xs
showPersistValue (PersistMap xs)              = T.unlines $ map (\(a,b) -> a <> ": " <> showPersistValue b) xs
showPersistValue (PersistObjectId _)          = "[object]"
showPersistValue (PersistDbSpecific _)        = "[binary db-specific]"

showPersistField :: (PersistField a) => a -> Text
showPersistField = showPersistValue . toPersistValue

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
