{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dics where

import Import_
import DicTypes
import qualified Data.Map as M
import qualified Data.Text as T
import Model
import Foundation_(AppMessage(..))
import Data.Char(toLower)
import Safe(readMay)

instance Read SomeDictionary where
    readsPrec _ = \s -> [(maybe (error "Can't parse Dictionary") id $ M.lookup (map toLower s) dics, "")]

instance PathPiece SomeDictionary where
    toPathPiece = T.pack . show
    fromPathPiece = readMay . T.unpack

dics :: M.Map String SomeDictionary
dics = M.fromList $ map (map toLower . show &&& id)
    [ SomeDictionary ([] :: [User])
    , SomeDictionary ([] :: [Email])
    ]

instance Default User
instance HasDictionary User where
    getDictionary
        = mkDic MsgUsers
            [ fld UserId
            , fld UserIdent     # label MsgIdent
            , fld UserPassword  # label MsgPassword
            ]
            # recShowField UserIdent

instance Default Email
instance HasDictionary Email where
    getDictionary
        = mkDic MsgEmails
            [ fld EmailId
            , fld EmailUser     # label MsgUser
            , fld EmailEmail    # label MsgEmail
            , fld EmailVerkey   # label MsgVerkey
                                # readonly
            ]
