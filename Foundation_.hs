module Foundation_
    ( module Foundation_
    ) where

import Import_
import Yesod.Static
import Yesod.Default.Config
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings (Extra (..))
import qualified Database.Persist
import Yesod.Core.Types (Logger)

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
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

