{-# LANGUAGE ScopedTypeVariables, RecordWildCards, MultiParamTypeClasses #-}
module Hap.Dictionary.ListHandler where

import Hap.Dictionary.Import
import qualified Data.Text as T
import Safe(readMay)

import Hap.Dictionary.Types
import Hap.Dictionary.Utils(pager, getRoot, showPersistValue, showPersistField, sortByPattern, widgetToHtml)
import Hap.Dictionary.Hap

getListR :: (YesodHap m) => SomeDictionary m -> HandlerT m IO Html
getListR sd@(SomeDictionary (_:: ([m],[a]))) = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET"
    cnt <- runDB $ count ([] :: [Filter a])
    $logDebug $ T.pack $ "cnt = " ++ show cnt
    mr <- getMessageRender
    root <- getRoot
    pgr <- newIdent
    defaultLayout $ do
        setTitleI $ MsgListDic $ mr dn
        [whamlet|
            <h1>_{MsgDictionary} #{mr dn}
            ^{pager pgr}
        |]
        toWidget [julius|
            showPager(#{toJSON $ listR root sd}, #{Number $ fromIntegral cnt});
            |]
  where
    dn  = case getDictionary :: Dictionary m a of
        (Dictionary {..}) -> dDisplayName


postListR :: (YesodHap m) => SomeDictionary m -> HandlerT m IO Html
postListR sd@(SomeDictionary (_ :: ([m],[a]))) = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "POST"
    (lim, off) <- fmap ((getParam 3 "lim" &&& getParam 0 "off") . fst) runRequestBody
    (res :: [Entity a]) <- runDB $ selectList [] [Asc persistIdField, OffsetBy off, LimitTo lim]
    let recs = map  (   entityKey
                    &&& map (showPersistValue . snd)
                        . sortByPattern getDBName fst (dFields dic)
                        . (\(PersistMap m) -> m) . toPersistValue . entityVal
                    ) res
    root <- getRoot
    widgetToHtml $ do
        [whamlet|
            <table width=100% border=1px>
                <tr>
                    <th align=center bgcolor=blue width=20px>
                        <a href=#{editR root sd defKey}>+
                    $forall df <- dFields dic
                        <th  bgcolor=blue>_{maybe (fsLabel (dfSettings df)) SomeMessage (dfShort df)}
                    <th align=center bgcolor=blue width=20px>-
                $forall (key,rec) <- recs
                    <tr>
                        <td align=center>
                            <a href=#{editR root sd (toPersistValue key)}>e
                        <td .key>#{showPersistField key}
                        $forall x <- rec
                                <td align=left>#{x}
                        <td align=center>
                            <button onclick=pdel('#{editR root sd (toPersistValue key)}','#{listR root sd}')>-
            |]
  where
    dic = getDictionary :: Dictionary m a
    defKey = toPersistValue (def :: Key a)
    getParam (v::Int) nm ps = fromMaybe v $ lookup nm ps >>= readMay . T.unpack

