{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
module Hap.Dictionary.EditHandler(getEditR, postEditR, deleteEditR) where
    
import Hap.Dictionary.Import
import qualified Data.Text as T

import Hap.Dictionary.Types
import Hap.Dictionary.Hap
import Hap.Dictionary.Utils

getEditR :: (YesodHap m) => SomeDictionary m -> PersistValue -> HandlerT m IO Html
getEditR sd@(SomeDictionary (_ :: ([m],[a]))) v = withEntity (getDictionary :: Dictionary m a) v produce
  where
    produce dicName ent = do
        $logDebug $ "entity: " <> T.pack (show ent)
        (widget, enctype) <- generateFormPost $ renderTable $ dictionaryAForm $ Just ent
        editForm (dicName <> ": " <> toPathPiece v) sd v widget enctype

editForm :: Yesod m => Text -> SomeDictionary m -> PersistValue -> WidgetT m IO () -> Enctype -> HandlerT m IO Html
editForm title sd v widget enctype =
    defaultLayout $ do
        setTitle $ toHtml title
        root <- getRoot
        let edR = editR root sd v
            lstR = listR root sd
            newR = editR root sd $ PersistInt64 (-1)
        [whamlet|
            <form method=post action=#{edR} enctype=#{enctype}>
                <table>
                    ^{widget}
                <span>
                    <button>Submit
                    <button type=button onclick=submit();window.open('#{newR}')>Add
                    <button type=button onclick=window.open('#{lstR}')>Close
                    <button type=button onclick=del()>Delete
        |]

        toWidget [julius|
                function del() {
                    $.ajax({
                        type: "DELETE"
                        , url: #{toJSON edR}
                        , success: function () {window.location=#{toJSON lstR}}
                    });
                }
            |]



withEntity  :: (HasDictionary m e, YesodHap m) 
    => Dictionary m e -> PersistValue -> (Text -> Entity e -> HandlerT m IO Html) 
    -> HandlerT m IO Html
withEntity (dic :: Dictionary m a) v produce = getMessageRender >>= withMR
  where
    ek = fromPersistValue v :: Either Text (Key a)
    withMR mr
        = either    ( showErr dicName . MsgInvalidKey dicName (toPathPiece v) )
                    ( \k -> do
                        me <- if k == def
                            then return $ Just def
                            else runDB $ get k
                        maybe   ( showErr dicName $ MsgNotFound dicName $ toPathPiece v )
                                ( produce dicName . Entity k )
                                me
                    )
                    ek
      where
        dicName = mr $ dDisplayName dic

showErr :: (RenderMessage site a, Yesod site) => Text -> a -> HandlerT site IO Html
showErr dicName mess = do
    setMessageWidget [whamlet|
        <h3>_{mess}
        <hr>|]
    defaultLayout $ do
        setTitle $ toHtml $ dicName <> " - error"

postEditR :: YesodHap m => SomeDictionary m -> PersistValue -> HandlerT m IO Html
postEditR sd@(SomeDictionary (_:: ([m],[a]))) v
    = withEntity (getDictionary :: Dictionary m a) v produce
  where
    produce _ ent = do
        ((result, _), _) <- runFormPost $ renderTable $ dictionaryAForm $ Just ent
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
                redirect $ editR root sd $ toPersistValue k'
            FormFailure xs -> do
                setMessageWidget [whamlet|
                        <h2 .error>_{MsgError $ T.unlines xs}
                        <hr>
                    |]
--                 editForm (dicName <> ": " <> toPathPiece v) sd v widget enctype
                redirect $ editR root sd v
            FormMissing -> do
                setMessageWidget [whamlet|
                        <h2 .error>_{MsgError "Missing form"}
                        <hr>
                    |]
                redirect $ editR root sd v

deleteEditR :: YesodHap m => SomeDictionary m -> PersistValue -> HandlerT m IO Html
deleteEditR sd@(SomeDictionary (_ :: ([m],[a]))) v = do
    mr <- getMessageRender
    let dicName = mr $ dDisplayName (getDictionary :: Dictionary m a)
    root <- getRoot
    either  ( showErr dicName . MsgInvalidKey dicName (toPathPiece v) )
            ( \k -> runDB (delete k) >> redirect (listR root sd) )
            (fromPersistValue v :: Either Text (Key a))
