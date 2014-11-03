{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}
module Hap.Dictionary.Utils where

import Hap.Dictionary.Import
import Control.Monad(liftM2)
import Control.Monad.Catch(MonadThrow)
import Data.List(sortBy)
import qualified Data.Map as M
import Data.Ord(comparing)
import qualified Data.Text as T
import Data.Time(formatTime)
import System.Locale(defaultTimeLocale)

import Hap.Dictionary.Hap

showPersistValue :: PersistValue -> Text
showPersistValue (PersistText v)              = v
showPersistValue (PersistByteString _)        = "[binary]"
showPersistValue (PersistInt64 v)             = T.pack $ show v
showPersistValue (PersistDouble v)            = T.pack $ show v
showPersistValue (PersistRational v)          = T.pack $ show v
showPersistValue (PersistBool b)              = if b then "+" else "-"
showPersistValue (PersistDay v)               = T.pack $ formatTime defaultTimeLocale "%x" v
showPersistValue (PersistTimeOfDay v)         = T.pack $ formatTime defaultTimeLocale "%X" v
showPersistValue (PersistUTCTime v)           = T.pack $ formatTime defaultTimeLocale "%x %X" v
showPersistValue PersistNull                  = mempty
showPersistValue (PersistList xs)             = T.unlines $ map showPersistValue xs
showPersistValue (PersistMap xs)              = T.unlines $ map (\(a,b) -> a <> ": " <> showPersistValue b) xs
showPersistValue (PersistObjectId _)          = "[object]"
showPersistValue (PersistDbSpecific _)        = "[binary db-specific]"

showPersistField :: (PersistField a) => a -> Text
showPersistField = showPersistValue . toPersistValue

entityFieldToPersist :: PersistEntity e => EntityField e t -> Entity e -> SomePersistField
entityFieldToPersist ef (Entity key (ent :: e))
    | persistFieldDef ef == persistFieldDef (persistIdField :: EntityField e (Key e))
        = SomePersistField key
    | otherwise = maybe (error $ unlines $
                                [ "Panic! Can't find entity field in function entityFieldToPersist"
                                , "persistFieldDef ef = " ++ show (persistFieldDef ef)
                                , "entityFields $ entityDef [ent] = ["
                                ]
                                ++ map show (entityFields $ entityDef [ent])
                                ++ ["]"]
            ) id
    $ lookup (persistFieldDef ef)
    $ zip (entityFields $ entityDef [ent]) $ toPersistFields ent

showEF :: PersistEntity e => EntityField e t -> Entity e -> Text
showEF ef = showPersistField . entityFieldToPersist ef

sortByPattern :: Ord a => (b -> a) -> (c -> a) -> [b] -> [c] -> [c]
sortByPattern f g ps = sortBy (comparing $ flip M.lookup m . g)
  where
    m = M.fromList $ zip (map f ps) [(1::Int)..]

getRoot :: (MonadHandler f, Yesod (HandlerSite f)) => f Text
getRoot = case approot of
    ApprootMaster f -> fmap f getYesod
    ApprootStatic t -> return t
    ApprootRelative -> return ".."
    ApprootRequest f -> liftM2 f getYesod (fmap reqWaiRequest getRequest)

widgetToHtml :: (Yesod site) => WidgetT site IO () -> HandlerT site IO Html
widgetToHtml = fmap pageBody . widgetToPageContent >=> withUrlRenderer

setMessageWidget :: (Yesod site) => WidgetT site IO () -> HandlerT site IO ()
setMessageWidget = widgetToHtml >=> setMessage

pager  :: (MonadThrow m, RenderMessage site HapMessage, MonadBaseControl IO m, MonadIO m)
       => Text -> WidgetT site m ()
pager pgr = do
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
        <div ##{pgr} hidden .pager>
            <div .pager-buttons>
                <button .first-btn .pager-btn title=_{MsgGoToFirstBtn}><<
                <button .prev-btn  .pager-btn title=_{MsgPrevPageBtn}><
                <span   .pager-text>_{MsgPage}
                <input  .pgnum .pager-input value=1>
                <span   .pager-text>_{MsgOf}
                <span   .pgcnt>..
                <button .next-btn  .pager-btn title=_{MsgNextPageBtn}>>
                <button .last-btn  .pager-btn title=_{MsgGoToLastBtn}>>>
                <span   .pager-text>_{MsgPageSize}
                <input  .pgsz .pager-input>
                <span   .pager-text>_{MsgRecCount}
                <span   .reccnt>
            <div .pager-content>
    |]
    let pgrJs = toJSON $ "#" <> pgr
    toWidget
        [julius|
            function pageSize() {
                return parseInt($('.pgsz', $(#{pgrJs}))[0].value);
            }
            function pageNum() {
                return parseInt($('.pgnum', $(#{pgrJs}))[0].value);
            }
            function pdel(urlVal,lstRoute) {
                $.ajax( { type: "DELETE"
                        , url: urlVal
                        , success: function () {
                                var cnt$ = $('.reccnt', $(#{pgrJs}));
                                cnt$.text(cnt$.text() - 1);
                                var ps = pageSize();
                                $('.pager-content', #{pgrJs}).load(lstRoute, {lim: ps, off: (pageNum()-1)*ps});
                            }
                        });
            }           
            function showPager(addr, cnt) {

                var pgr$ = $(#{pgrJs})
                    , bFst  = $('.first-btn', pgr$)[0]
                    , bLst  = $('.last-btn' , pgr$)[0]
                    , bPrev = $('.prev-btn' , pgr$)[0]
                    , bNext = $('.next-btn' , pgr$)[0]
                    , ePn   = $('.pgnum'    , pgr$)[0]
                    , ePc   = $('.pgcnt'    , pgr$)[0]
                    , tab$  = $('.pager-content', pgr$)
                    , ps    = localStorage["pageSize"];
                ps = !!ps ? ps : '20';
                $('.reccnt', pgr$).text(cnt);

                function loadData() {
                    var ps=pageSize();
                    var pcnt = Math.ceil(cnt / ps);
                    localStorage["pageSize"] = ps;
                    if (ePc.innerText) ePc.innerText = pcnt;
                    else ePc.innerHTML = pcnt;
                    var cp=parseInt(ePn.value);
                    ePn.value = cp = cp < 0 ? pcnt : Math.min(pcnt, Math.max(1, cp));
                    tab$.load(addr, {lim: ps, off: (cp-1)*ps});
                    bFst.disabled = bPrev.disabled = cp == 1;
                    bLst.disabled = bNext.disabled = cp == pcnt;
                }
                bFst.act  = function()  { return 1; };
                bNext.act = function(v) { return v+1; };
                bPrev.act = function(v) { return v-1; };
                bLst.act  = function() { return -1; };
                $(".pager-btn", pgr$).click(function() {
                    ePn.value = this.act(parseInt(ePn.value));
                    loadData();
                });
                $('.pgnum', pgr$).change(loadData);
                $('.pgsz' , pgr$).val(ps).change(loadData);
                loadData(); 
                return pgr$.show();
            }
            |]
