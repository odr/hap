{-
module Dics where

import Data.Char(toLower)
import qualified Data.Map as M

import Hap.Dictionary.EDSL

import Model
import Foundation_
-}

instance HasMapDict App where
    getMapDict =  M.fromList $ map (map toLower . show &&& id)
        [ someDic ([] :: [User])
        , someDic ([] :: [Email])
        ]
instance Default User
instance HasDictionary App User where
    getDictionary
        = mkDic MsgUsers 
            ( fld UserIdent     )
            [ fld UserIdent     # label MsgIdent
            , fld UserPassword  # label MsgPassword
            ]
            # recShowField UserIdent

instance Default Email
instance HasDictionary App Email where
    getDictionary
        = mkDic MsgEmails 
            ( fld EmailId       )
            [ fld EmailUser     # label MsgUser
            , fld EmailEmail    # label MsgEmail
            , fld EmailVerkey   # label MsgVerkey
                                # readonly
            ]
