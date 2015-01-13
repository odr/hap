{-# LANGUAGE TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}
module App where

import Settings              
import Database.Persist.Sql (ConnectionPool)
import Hap.Dictionary.Import
import Network.HTTP.Client.Conduit(Manager)
import Yesod.Static

import Hap.Dictionary.Hap(Hap, HapMessage)
import Yesod.Core.Types (Logger)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , getHap :: Hap
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage App HapMessage where
    renderMessage = renderMessage . getHap

-- type SomeDictionary' = SomeDictionary App

