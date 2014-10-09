{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Edit(getEditR, postEditR, deleteEditR) where

import Import
import qualified Data.Text as T

getEditR :: SomeDictionary -> PersistValue -> Handler Html
getEditR sd@(SomeDictionary (_::[a])) v = withEntity (getDictionary :: Dictionary a) v produce
  where
    produce dicName ent = do
        (widget, enctype) <- generateFormPost $ renderTable $ dictionaryAForm $ Just ent
        editForm (dicName <> ": " <> toPathPiece v) sd v widget enctype

editForm :: Text -> SomeDictionary -> PersistValue -> Widget -> Enctype -> Handler Html
editForm title sd v widget enctype =
    defaultLayout $ do
        setTitle $ toHtml title
        [whamlet|
            <form method=post action=@{EditR sd v} enctype=#{enctype}>
                <table>
                    ^{widget}
                <span>
                    <button>Submit
                    <a href=@{EditR sd (PersistInt64 -1)}>Add
                    <a href=@{ListR sd}>Close
        |]


withEntity :: (HasDictionary e) => Dictionary e -> PersistValue -> (Text -> Entity e -> Handler Html) -> Handler Html
withEntity (dic :: Dictionary a) v produce = getMessageRender >>= withMR
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

showErr dicName mess = do
    setMessageWidget [whamlet|
        <h3>_{mess}
        <hr>|]
    defaultLayout $ do
        setTitle $ toHtml $ dicName <> " - error"

postEditR :: SomeDictionary -> PersistValue -> Handler Html
postEditR sd@(SomeDictionary (_::[a])) v
    = withEntity (getDictionary :: Dictionary a) v produce
  where
    produce _ ent = do
        ((result, _), _) <- runFormPost $ renderTable $ dictionaryAForm $ Just ent
        case result of
            FormSuccess (Entity k e) -> do

                k' <- if k == def
                    then runDB $ insert e
                    else runDB (replace k e) >> return k

                setMessageWidget [whamlet|
                        <h2 .info>_{MsgSaved}
                        <hr>
                    |]
                redirect $ EditR sd $ toPersistValue k'
            FormFailure xs -> do
                setMessageWidget [whamlet|
                        <h2 .error>_{MsgError $ T.unlines xs}
                        <hr>
                    |]
--                 editForm (dicName <> ": " <> toPathPiece v) sd v widget enctype
                redirect $ EditR sd v
            FormMissing -> do
                setMessageWidget [whamlet|
                        <h2 .error>_{MsgError "Missing form"}
                        <hr>
                    |]
                redirect $ EditR sd v

deleteEditR :: SomeDictionary -> PersistValue -> Handler Html
deleteEditR sd@(SomeDictionary (_::[a])) v = do
    mr <- getMessageRender
    let dicName = mr $ dDisplayName (getDictionary :: Dictionary a)
    either  ( showErr dicName . MsgInvalidKey dicName (toPathPiece v) )
            ( \k -> runDB (delete k) >> redirect (ListR sd) )
            (fromPersistValue v :: Either Text (Key a))
