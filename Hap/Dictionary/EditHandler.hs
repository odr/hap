{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
module Hap.Dictionary.EditHandler(getEditR, postEditR, deleteEditR) where
    
import Hap.Dictionary.Import
import Control.Monad(replicateM)
import qualified Data.Text as T


import Hap.Dictionary.Types
import Hap.Dictionary.Hap
import Hap.Dictionary.Pager
import Hap.Dictionary.Utils(getRoot, setMessageWidget)

getEditR :: (YesodHap m) => SomeDictionary m -> PersistValue -> HandlerT m IO Html
getEditR sd@(SomeDictionary (_ :: ([m],[a]))) v 
    = fmap (fromMaybe mempty) $ withEntity (getDictionary :: Dictionary m a) v produce
  where
    produce dicName ent = do
        $logDebug $ "entity: " <> T.pack (show ent)
        (widget, enctype) <- generateFormPost $ renderTable $ dictionaryAForm $ Just ent
        let title = dicName <> ": " <> toPathPiece v
        root <- getRoot
        let edR = editR root sd v
            lstR = listR root sd
        [selId, formId, editorId] <- replicateM 3 newIdent
        defaultLayout $ do
            setTitle $ toHtml title
            toWidget [cassius|
                    .cell-editor
                        padding-right: 10px
                        width: 50%
                    .cell-selector
                        padding-left: 10px
                |]
            [whamlet|
                <h1>#{title}
                <table>
                    <tr>
                        <td .cell-editor >
                            <div ##{editorId} onclick=hidePager(this)>
                                <form ##{formId} method=post action=#{edR} enctype=#{enctype}>
                                    <table>
                                        ^{widget}
                                    <span>
                                        <button type=submit >Submit
                                        $#<button type=submit onclick=sub('#{newR}')>Add
                                        <button type=button onclick=window.location='#{lstR}'>Close
                                        <button type=button onclick=del()>Delete
                        <td #selId .cell-selector>
                            ^{pager}
                            <button type="button" onclick="selectAndHidePager()">_{MsgSelect}
            |]
            toWidget [julius|
                function del() {
                    $.ajax({
                        type: "DELETE"
                        , url: #{toJSON edR}
                        , success: function () {window.location=#{toJSON lstR}}
                    });                
                }
                function hidePager() {
                    $(#{toJSON $ "#" <> selId}).hide();
                }

            |]

withEntity  :: (HasDictionary m e, YesodHap m, ToTypedContent a) 
    => Dictionary m e -> PersistValue -> (Text -> Entity e -> HandlerT m IO a) 
    -> HandlerT m IO (Maybe a)
withEntity (dic :: Dictionary m a) v produce = getMessageRender >>= withMR
  where
    ek = fromPersistValue v :: Either Text (Key a)
    withMR mr
        = either    ( \t -> showErr (MsgInvalidKey dicName (toPathPiece v) t) >> return Nothing )
                    ( \k -> do
                        me <- if k == def
                            then return $ Just def
                            else runDB $ get k
                        maybe   ( showErr (MsgNotFound dicName $ toPathPiece v) >> return Nothing )
                                ( fmap Just . produce dicName . Entity k )
                                me
                    )
                    ek
      where
        dicName = mr $ dDisplayName dic

showErr :: (RenderMessage site a, Yesod site) => a -> HandlerT site IO ()
showErr mess = setMessageWidget [whamlet|
        <h3>_{mess}
        <hr>|]

postEditR :: YesodHap m => SomeDictionary m -> PersistValue -> HandlerT m IO Value
postEditR sd@(SomeDictionary (_:: ([m],[a]))) v
    = fmap (fromMaybe Null) $ withEntity (getDictionary :: Dictionary m a) v produce
  where
    produce _ ent = do
        $logDebug $ debugMess "postEditR: ent = {}" $ Only $ Shown ent
        ((result, _), _) <- runFormPost $ renderTable $ dictionaryAForm $ Just ent
        $logDebug $ debugMess "postEditR: result = {}" $ Only $ Shown result
        root <- getRoot
        case result of
            FormSuccess (Entity k e) -> do

                k' <- if k == def
                    then runDB $ insert e
                    else runDB (replace k e) >> return k

                setMessageWidget [whamlet|
                        <h2 .info>_{MsgSaved}
                        <hr>
                    |]
                _ <- redirect $ editR root sd $ toPersistValue k'
                return $ toJSON k'
            FormFailure xs -> do
                setMessageWidget [whamlet|
                        <h2 .error>_{MsgError $ T.unlines xs}
                        <hr>
                    |]
                _ <- redirect $ editR root sd v
                return Null
--                 editForm (dicName <> ": " <> toPathPiece v) sd v widget enctype
            FormMissing -> do
                setMessageWidget [whamlet|
                        <h2 .error>_{MsgError "Missing form"}
                        <hr>
                    |]
                _ <- redirect $ editR root sd v
                return Null

deleteEditR :: YesodHap m => SomeDictionary m -> PersistValue -> HandlerT m IO Value
deleteEditR (SomeDictionary (_ :: ([m],[a]))) v = do
    mr <- getMessageRender
    let dicName = mr $ dDisplayName (getDictionary :: Dictionary m a)
    -- root <- getRoot
    either  ( \t -> showErr (MsgInvalidKey dicName (toPathPiece v) t) >> return (Bool False) )
            ( \k -> runDB (delete k) >> return (Bool True)) -- >> redirect (listR root sd) )
            (fromPersistValue v :: Either Text (Key a))
