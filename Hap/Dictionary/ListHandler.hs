{-# LANGUAGE ScopedTypeVariables, RecordWildCards, MultiParamTypeClasses #-}
module Hap.Dictionary.ListHandler where

import Import_
import qualified Data.Text as T
-- import Yesod(RenderRoute(..))
import Hap.Dictionary.Types
import Hap.Dictionary.Hap
import Hap.Dictionary.Utils
import Safe(readMay)
import Control.Monad.Catch(MonadThrow)

getListR :: SomeDictionary m -> HandlerT m IO Html
getListR sd@(SomeDictionary (_:: ([m],[a]))) = do
    cnt <- runDB $ count ([] :: [Filter a])
    $logDebug $ T.pack $ "cnt = " ++ show cnt
    defaultLayout $ do
        mr <- getMessageRender
        setTitleI $ MsgListDic $ mr dn
        root <- getRoot
        [whamlet|
            <h1>_{MsgDictionary} #{mr dn}
            ^{pager (listR root sd) cnt}
            <div #lstTab>
            |]
        toWidget [julius|
                function del(urlVal) {
                    $.ajax({
                        type: "DELETE"
                        , url: urlVal
                        , success: function(msg){
                            alert("Data Deleted: " + msg);
                        }
                    });
                }
            |]
  where
    dn  = case getDictionary :: Dictionary m a of
        (Dictionary {..}) -> dDisplayName


postListR :: SomeDictionary m -> HandlerT m IO Html
postListR sd@(SomeDictionary (_ :: ([m],[a]))) = do
    -- addHeader "Access-Control-Allow-Origin" "*"
    -- addHeader "Access-Control-Allow-Methods" "POST"
    rds <- fmap fst runRequestBody
    let lim = maybe (3::Int) id $ lookup "lim" rds >>= readMay . T.unpack
        off = maybe (0::Int) id $ lookup "off" rds >>= readMay . T.unpack
    (res :: [Entity a]) <- runDB $ selectList [] [Asc persistIdField, OffsetBy off, LimitTo lim]
    let recs = map  (   entityKey
                    &&& map (showPersistValue . snd)
                        . sortByPattern getDBName fst (dFields dic)
                        . (\(PersistMap m) -> m) . toPersistValue . entityVal
                    ) res
    -- $logDebug $ T.unlines $ map (T.pack . show) recs
    --                 -- <th bgcolor=blue>_{MsgId}
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
                            <button onclick=alert('del');del('#{editR root sd (toPersistValue key)}')>-
            |]
  where
    dic = getDictionary :: Dictionary m a
    defKey = toPersistValue (def :: Key a)

pager 	:: (RenderMessage site HapMessage, ToJSON a, MonadThrow m, MonadIO m, MonadBaseControl IO m) 
		=> a -> Int -> WidgetT site m ()
pager route cnt = do
    toWidget [cassius|
        .pager
            border: ridge
            padding: 3px
            text-align: left
        .pager-text
            padding-left: 1%
            padding-right: 1%
        .pager-input
            width: 40px
            text-align: right
        .container
            width: 1280px
        th
            color: yellow
        |]
    [whamlet|
        <div .pager>
            <button #goFstBtn  .pager-btn title=_{MsgGoToFirstBtn}><<
            <button #goPrevBtn .pager-btn title=_{MsgPrevPageBtn}><
            <span .pager-text>_{MsgPage}
            <input #pgnum .pager-input value=1>
            <span .pager-text>_{MsgOf}
            <span #pgcnt>..
            <button #goNextBtn .pager-btn title=_{MsgNextPageBtn}>>
            <button #goLstBtn  .pager-btn title=_{MsgGoToLastBtn}>>>
            <span .pager-text>_{MsgPageSize}
            <input #pgsz .pager-input value=#{pgsz}>
            <span .pager-text>_{MsgRecCount cnt}
        |]
    toWidget
        [julius|
            function addPager(pager$, tab$, addr, cntRec) {
                var bFst = $('#goFstBtn', pager$)[0];
                var bLst = $('#goLstBtn', pager$)[0];
                var bNext = $('#goNextBtn', pager$)[0];
                var bPrev = $('#goPrevBtn', pager$)[0];
                var ePn = $('#pgnum', pager$)[0];
                var ePs = $('#pgsz', pager$)[0];
                var ePc = $('#pgcnt', pager$)[0];

                function loadData() {
                    var ps=parseInt(ePs.value);
                    var pcnt = Math.ceil(cntRec / ps);
                    if (ePc.innerText) ePc.innerText = pcnt;
                    else ePc.innerHTML = pcnt;
                    var cp=parseInt(ePn.value);
                    ePn.value = cp = cp < 0 ? pcnt : Math.min(pcnt, Math.max(1, cp));
                    tab$.load(addr, {lim: ps, off: (cp-1)*ps});
                    bFst.disabled = bPrev.disabled = cp == 1;
                    bLst.disabled = bNext.disabled = cp == pcnt;
                }
                onload = onload ? function() { onload(); loadData(); } : loadData;
                bFst.act  = function()  { return 1; };
                bNext.act = function(v) { return v+1; };
                bPrev.act = function(v) { return v-1; };
                bLst.act  = function() { return -1; };
                $(".pager-btn").click(function() {
                    ePn.value = this.act(parseInt(ePn.value));
                    loadData();
                });
                $("#pgnum").change(loadData);
                $("#pgsz").change(loadData);
            }
            addPager($(".pager"), $('#lstTab'), #{toJSON route}, #{Number $ fromIntegral cnt});
            |]
  where
    pgsz = 15::Int



