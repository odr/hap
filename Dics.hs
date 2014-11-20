{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dics where

import Data.Char(toLower)
import qualified Data.Map as M

import Hap.Dictionary.EDSL

import Model
import Foundation

instance HasMapDict App where
    getMapDict =  M.fromList $ map (map toLower . show &&& id)
        [ someDic (def :: [User])
        , someDic (def :: [Email])
        ]
instance Default User
instance HasDictionary App User where
    getDictionary
        = mkDic MsgUsers 
            -- ( fld UserIdent     )
            ( Vertical $ pure <$>
                [ fld UserIdent     # label MsgIdent
                , ref def EmailUser
                , fld UserPassword  # label MsgPassword
                ]
            )
            # recShowField UserIdent

instance Default Email
instance HasDictionary App Email where
    getDictionary
        = mkDic MsgEmails 
            -- ( fld EmailId       )
            ( Vertical $ pure <$>
                [ fld EmailUser     # label MsgUser
                , fld EmailEmail    # label MsgEmail
                , fld EmailVerkey   # label MsgVerkey
                                    # readonly
                ]
            )
