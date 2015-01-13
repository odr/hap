{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
module Hap.Dictionary.EditHandler(getEditR, postEditR, deleteEditR) where
    
import Hap.Dictionary.Import
-- import Control.Monad(replicateM)
import qualified Data.Text as T


import Hap.Dictionary.Types
import Hap.Dictionary.Hap
import Hap.Dictionary.Pager
import Hap.Dictionary.Form 
import Hap.Dictionary.EntityPlus
import Hap.Dictionary.Utils(getRoot, setMessageWidget)
import qualified Data.Set as S
import Control.Monad.Trans.Except(runExceptT)

getEditR :: (YesodHap m) => SomeDictionary m -> PersistValue -> HandlerT m IO Html
getEditR (sd@(SomeDictionary (_ :: [a])) :: SomeDictionary m) v 
    = fmap (fromMaybe mempty) $ withEntityPlus (getDictionary :: Dictionary m a) v (editEP sd v)
  
editEP :: (HasDictionary m e, YesodHap m) 
    => SomeDictionary m -> PersistValue -> EntityPlus m e -> HandlerT m IO Html
editEP (sd@(SomeDictionary (_ :: [a])) :: SomeDictionary m) v ent = do
    $logDebug $ "entity: " <> T.pack (show ent)
    (widget, enctype) <- generateFormPost $ entityPlusMForm ent
{-
        showForm sd v widget enctype

showForm :: (YesodHap m) => SomeDictionary m -> PersistValue -> WidgetT m IO () -> Enctype -> HandlerT m IO Html
showForm sd@(SomeDictionary (_::[a]) :: SomeDictionary m) v widget enctype = do
-}
    dicName <- (dDisplayName (getDictionary :: Dictionary m a) #) <$> getMessageRender
    let title = dicName <> ": " <> toPathPiece v
    root <- getRoot
    let edR = editR root sd v
        lstR = listR root sd
    [selId, formId, editorId] <- replicateM 3 newIdent
    defaultLayout $ do
        setTitle $ toHtml title
        toWidget [cassius|
                .container
                    width: 2000px
                .cell-editor
                    padding-right: 10px
                .cell-selector
                    padding-left: 10px
            |]
        [whamlet|
            <h1>#{title}
            <table>
                <tr>
                    <td .cell-editor>
                        <div ##{editorId} onclick=hidePager(this) display=inline>
                            <form ##{formId} method=post enctype=#{enctype}>
                                ^{widget}
                                <div display=inline>
                                    <button type=submit >Submit
                                    <button type=button onclick=window.location='#{lstR}'>To List
                                    <button type=button onclick=editor_del()>Delete
                    <td ##{selId} .cell-selector hidden>
                        ^{pager $ Just selId}
        |]
        toWidget [julius|
            function editor_del() {
                $.ajax({
                    type: "DELETE"
                    , url: #{toJSON edR}
                    , success: function () {window.location=#{toJSON lstR}}
                });                
            }
        |]


withEntityPlus  :: (HasDictionary m e, YesodHap m, ToTypedContent a) 
    => Dictionary m e -> PersistValue -> (EntityPlus m e -> HandlerT m IO a) 
    -> HandlerT m IO (Maybe a)
withEntityPlus (dic :: Dictionary m a) v produce = getMessageRender >>= withMR
  where
    ek = fromPersistValue v :: Either Text (Key a)
    withMR mr
        = either    ( \t -> showErr (MsgInvalidKey dicName (toPathPiece v) t) >> return Nothing )
                    ( \k -> fmap Just . produce
                            =<< if k == def 
                                    then return def
                                    else runDB $ getEntityPlus dic k
                        {-
                        maybe   ( showErr (MsgNotFound dicName $ toPathPiece v) >> return Nothing )
                                ( fmap Just . produce dicName . Entity k )
                                me
                        -}
                    )
                    ek
      where
        dicName = mr $ dDisplayName dic

showErr :: (RenderMessage site a, Yesod site) => a -> HandlerT site IO ()
showErr mess = setMessageWidget [whamlet|
        <h3>_{mess}
        <hr>|]

postEditR :: YesodHap m => SomeDictionary m -> PersistValue -> HandlerT m IO Html
postEditR (sd@(SomeDictionary (_ :: [a])) :: SomeDictionary m) v
    = fmap (fromMaybe mempty) $ withEntityPlus (getDictionary :: Dictionary m a) v produce
  where
    produce ep = do
        $logDebug $ debugMess "postEditR: ep = {}" $ Only $ Shown ep
        ((result, _), _) <- runFormPost $ entityPlusMForm ep
        $logDebug $ debugMess "postEditR: result = {}" $ Only $ Shown result
        root <- getRoot
        case result of
            FormSuccess rep -> do
                eep <- runDB $ runExceptT $ putEntityPlus IgnoreNothing Nothing rep
                either
                    (\errs -> do
                        let vals = map (first $ (==ValidationError) &&& (== ValidationWarning)) $ S.toList errs
                        setMessageWidget [whamlet|
                                $forall ((isErr, isWarn),txt) <- vals                                
                                    <h4 :isErr:.validation-error 
                                            :isWarn:.validation-warning>#{txt}
                                <hr>
                            |]
                        editEP sd (toPersistValue $ entityKey $ _epEntity rep) rep
                        -- redirect $ editR root sd $ toPersistValue $ entityKey $ _epEntity rep
                    )
                    (\ep' -> do
                        setMessageWidget [whamlet|
                                <h4 .info>_{MsgSaved}
                                <hr>
                            |]
                        redirect $ editR root sd $ toPersistValue $ entityKey $ _epEntity ep'
                    )
                    eep  

            FormFailure xs -> do
                setMessageWidget [whamlet|
                        $forall e <- xs
                            <h4 .error>_{MsgError e}
                        <hr>
                    |]
                redirect $ editR root sd v
--                return Null
--                 editForm (dicName <> ": " <> toPathPiece v) sd v widget enctype
            FormMissing -> do
                setMessageWidget [whamlet|
                        <h4 .error>_{MsgError "Missing form"}
                        <hr>
                    |]
                redirect $ editR root sd v
                -- return Null

deleteEditR :: YesodHap m => SomeDictionary m -> PersistValue -> HandlerT m IO Value
deleteEditR (SomeDictionary (_ :: [a]) :: SomeDictionary m) v = do
    mr <- getMessageRender
    let dicName = mr $ dDisplayName (getDictionary :: Dictionary m a)
    -- root <- getRoot
    either  ( \t -> showErr (MsgInvalidKey dicName (toPathPiece v) t) >> return (Bool False) )
            ( \k -> runDB (delete k) >> return (Bool True)) -- >> redirect (listR root sd) )
            (fromPersistValue v :: Either Text (Key a))
