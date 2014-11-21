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
            ( Vertical 
                [ Horizontal $ pure <$> 
                    [ fld UserIdent     # label MsgIdent
                    , fld UserPassword  # label MsgPassword 
                    ]
                , ref def EmailUser # pure
                , Horizontal $ pure <$>
                    [ fld UserEmployment    # label MsgEmployment
                    , fld UserFamilyStatus  # label MsgFamilyStatus
                    ]
                ]
            )
            # recShowField UserIdent

instance Default Email
instance HasDictionary App Email where
    getDictionary
        = mkDic MsgEmails 
            ( Vertical 
                [ fld EmailUser     # label MsgUser # pure
                , Horizontal $ pure <$>
                    [ fld EmailEmail    # label MsgEmail
                    , fld EmailVerkey   # label MsgVerkey
                                        # readonly
                    ]
                ]
            )
