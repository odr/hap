{-# LANGUAGE ScopedTypeVariables #-}
module Handler.List(getListR, postListR) where

import Import
import Yesod(RenderRoute(..))
import Hap.Dictionary.Utils(showPersistValue, sortByPattern, showPersistField)
import qualified Data.Text as T
--import qualified Data.Map as M
import Safe(readMay)

getListR :: SomeDictionary -> Handler Html
getListR sd@(SomeDictionary (_::[a])) = do
    cnt <- runDB $ count ([] :: [Filter a])
    $logDebug $ T.pack $ "cnt = " ++ show cnt
    defaultLayout $ do
        (mr :: AppMessage -> Text) <- getMessageRender
        setTitleI $ MsgListDic $ mr $ dDisplayName dic
        [whamlet|
            <h1>_{MsgDictionary} "_{dDisplayName dic}"
            ^{pager (ListR sd) cnt}
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
    dic = getDictionary :: Dictionary a

postListR :: SomeDictionary -> Handler Html
postListR sd@(SomeDictionary (_ :: [a])) = do
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
    widgetToHtml $ do
        [whamlet|
            <table width=100% border=1px>
                <tr>
                    <th align=center bgcolor=blue width=20px>
                        <a href=@{EditR sd (toPersistValue defKey)}>+
                    $forall df <- dFields dic
                        <th  bgcolor=blue>_{maybe (fsLabel (dfSettings df)) SomeMessage (dfShort df)}
                    <th align=center bgcolor=blue width=20px>-
                $forall (key,rec) <- recs
                    <tr>
                        <td align=center>
                            <a href=@{EditR sd (toPersistValue key)}>e
                        <td .key>#{showPersistField key}
                        $forall x <- rec
                                <td align=left>#{x}
                        <td align=center>
                            <button onclick="alert("del");del(@{EditR sd (toPersistValue key)})">-
            |]
  where
    dic = getDictionary :: Dictionary a
    defKey = def :: Key a

pager :: Route App -> Int -> Widget
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
                var bFst = $('#goFstBtn')[0];
                var bLst = $('#goLstBtn')[0];
                var bNext = $('#goNextBtn')[0];
                var bPrev = $('#goPrevBtn')[0];
                var ePn = $('#pgnum')[0];
                var ePs = $('#pgsz')[0];
                var ePc = $('#pgcnt')[0];

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
            addPager($(".pager"), $('#lstTab'), "@{route}", #{Number $ fromIntegral cnt});
            |]
  where
    pgsz = 15::Int

