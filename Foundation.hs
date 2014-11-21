{-# LANGUAGE FlexibleInstances #-}
module Foundation (module App, module Foundation) where

import Hap.Dictionary.Import

import App

import qualified Data.Text as T
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod
import Yesod.Auth
--import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Yesod.Auth.BrowserId as BID
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.Jquery(YesodJquery(..))
import qualified Settings
import           Settings (widgetFile, Extra (..))
import Yesod.Static

import Hap.Dictionary.EDSL

import Model
-- import           Settings (widgetFile, Extra (..))
import Settings.Development (development)
import Settings.StaticFiles


-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                ])
            $logDebug $ either (T.pack . show) id $ urlJqueryJs master
            addScriptEither $ urlJqueryJs master
            addScriptEither $ urlJqueryUiJs master
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authenitcation.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal (if development then Right else minifym) genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

instance YesodJquery App

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuthPersist App

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert User
                    { userIdent = credsIdent creds
                    , userPassword = Nothing
                    , userEmployment = Employed
                    , userFamilyStatus = Nothing
                    }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [BID.authBrowserId BID.def]

    authHttpManager = httpManager

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
instance YesodHap App

-- #include "Dics.hs"
{-
instance HasMapDict App where
    getMapDict =  M.fromList $ map (map toLower . show &&& id)
        [ someDic ([] :: [User])
        , someDic ([] :: [Email])
        ]
instance Default User
instance HasDictionary App User where
    getDictionary
        = mkDic MsgUsers
            [ fld UserId
            , fld UserIdent     # label MsgIdent
            , fld UserPassword  # label MsgPassword
            ]
            # recShowField UserIdent

instance Default Email
instance HasDictionary App Email where
    getDictionary
        = mkDic MsgEmails
            [ fld EmailId
            , fld EmailUser     # label MsgUser
            , fld EmailEmail    # label MsgEmail
            , fld EmailVerkey   # label MsgVerkey
                                # readonly
            ]
-}