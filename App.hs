module App where

import Hap.Dictionary.Import
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import Yesod.Default.Config
import Yesod.Static
import qualified Settings
import           Settings (widgetFile, Extra (..))
import Hap.Dictionary.Hap(Hap, HapMessage)
import Yesod.Core.Types (Logger)
import qualified Database.Persist

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    , getHap :: Hap
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage App HapMessage where
    renderMessage = renderMessage . getHap

-- type SomeDictionary' = SomeDictionary App

