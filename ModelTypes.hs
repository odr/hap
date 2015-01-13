{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module ModelTypes where

import App
import Hap.Dictionary.Import
import Hap.Dictionary.FieldFormI()
import Hap.Dictionary.EDSL
import qualified Data.Char as C
import Database.Persist.Sql hiding (Single)
import qualified Data.Text as T
import Data.Fixed

--------------------------------------------------

data Employment = Employed | Unemployed | Retired
    deriving (Show, Read, Eq)
derivePersistField "Employment"
instance Default Employment where
    def = Unemployed

messEmployment  ::  [(AppMessage, Employment)]
messEmployment  =   [ (MsgEmploymentEmployed  , Employed)
                    , (MsgEmploymentUnemployed, Unemployed) 
                    , (MsgEmploymentRetired   , Retired)
                    ]
instance FieldForm App e Employment where
    fieldAForm = listFieldAForm messEmployment

instance FieldForm App e (Maybe Employment) where
    fieldAForm = listFieldAFormOpt messEmployment

instance FieldToText App Employment where
    fieldToText = listFieldToText messEmployment

--------------------------------------------------

data FamilyStatus = Single | Married | Devorsed | Widowed
    deriving (Show, Read, Eq)
derivePersistField "FamilyStatus"

messFamilyStatus :: [(AppMessage, FamilyStatus)]
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

-------------------------------------------------------    

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
    fromPersistValue v = Left $ "Invalid Price value: " <> T.pack (show v)

instance PersistFieldSql FPrice where
    sqlType _ = SqlNumeric 14 2 

-----------------------------------------------------

newtype EmailF = EmailF { unEmailF :: Text } deriving (Eq, Show)
instance PersistField EmailF where
    toPersistValue = PersistText . unEmailF
    fromPersistValue = fmap EmailF . fromPersistValue

instance PersistFieldSql EmailF where
    sqlType _ = SqlString

instance Default EmailF where
    def = EmailF def
checkedEmailField 
    :: (RenderMessage (HandlerSite m) AppMessage,
        RenderMessage (HandlerSite m) FormMessage, Monad m) 
    => Field m Text
checkedEmailField = check validateEmail emailField
  where
    validateEmail txt 
        | all ($ txt)   [ not . T.null . snd . T.breakOn (".") . snd . T.breakOn ("@") 
                        , (==Nothing) . T.find C.isSpace
                        ] = Right txt
        | otherwise = Left MsgInvalidEmailF

instance FieldForm App e EmailF where
    fieldAForm _ fs = fmap EmailF . areq checkedEmailField fs . fmap unEmailF

instance FieldForm App e (Maybe EmailF) where
    fieldAForm _ fs = fmap (fmap EmailF) . aopt checkedEmailField fs . fmap (fmap unEmailF)

instance FieldToText App EmailF where
    fieldToText = fieldToText . unEmailF


