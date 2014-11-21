{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module ModelTypes where

import App
import Hap.Dictionary.Import
import Hap.Dictionary.EDSL
import Database.Persist.TH
import Database.Persist.Sql hiding (Single)
import qualified Data.Text as T
import Data.Fixed
import Safe(readMay)

data Employment = Employed | Unemployed | Retired
    deriving (Show, Read, Eq)
derivePersistField "Employment"
instance Default Employment where
    def = Unemployed

messEmployment =    [ (MsgEmploymentEmployed  , Employed)
                    , (MsgEmploymentUnemployed, Unemployed) 
                    , (MsgEmploymentRetired   , Retired)
                    ]
instance FieldForm App e Employment where
    fieldAForm = listFieldAForm messEmployment

instance FieldForm App e (Maybe Employment) where
    fieldAForm = listFieldAFormOpt messEmployment

instance FieldToText App Employment where
    fieldToText = listFieldToText messEmployment

data FamilyStatus = Single | Married | Devorsed | Widowed
    deriving (Show, Read, Eq)
derivePersistField "FamilyStatus"

messFamilyStatus =  [ (MsgFamilyStatusSingle  , Single)
                    , (MsgFamilyStatusMarried , Married)
                    , (MsgFamilyStatusDevorsed, Devorsed)
                    , (MsgFamilyStatusWidowed , Widowed)
                    ]

instance FieldForm App e FamilyStatus where
    fieldAForm = listFieldAForm messFamilyStatus

instance FieldForm App e (Maybe FamilyStatus) where
    fieldAForm = listFieldAFormOpt messFamilyStatus

instance FieldToText App FamilyStatus where
    fieldToText = listFieldToText messFamilyStatus

newtype FPrice = FPrice { unPrice :: Fixed E2 } deriving (Eq, Show, Read)

instance PersistField FPrice where
    toPersistValue = PersistText . T.pack . show . unPrice
    fromPersistValue (PersistText txt) 
        | length s > 14 = Left $ "Length of value with type Price more than 14: " <> txt
        | otherwise     = maybe 
            (Left $ "Invalid Price value: " <> txt) 
            (Right . FPrice)
            $ readMay s
      where
        s = T.unpack txt
    fromPersistValue v = Left $ "Invalid Price value: " <> (T.pack $ show v)

instance PersistFieldSql FPrice where
    sqlType _ = SqlNumeric 14 2 


