module Hap.Dictionary.Pager where

import Hap.Dictionary.Import 
import qualified Data.Text as T
-- import Safe(readMay)
-- import Control.Monad.Catch(MonadThrow)

import Hap.Dictionary.Hap

data PagerParams = PagerParams 
    { ppLimit       :: Int
    , ppOffset      :: Int
    , ppIsSelect    :: Bool 
    , ppCount       :: Int    
    , ppIdRec       :: Text
    }
getPagerParams :: HandlerT m IO PagerParams
getPagerParams 
    = fmap (
        ( PagerParams
        <$> getI 3 "lim"
        <*> getI 0 "off"
        <*> getB "isSel"
        <*> getI 0 "cnt"
        <*> fromMaybe mempty . lookup "idRec"
        ) . fst) runRequestBody
  where
    getMI nm    = lookup nm >=> readMay . T.unpack
    getI v nm   = fromMaybe v . getMI nm
    getB nm     = maybe False (== "true") . lookup nm

pager  :: (MonadThrow m, RenderMessage site HapMessage, MonadBaseControl IO m, MonadIO m)
       => Maybe Text -> WidgetT site m ()
pager mbSelId = do
    pgr <- newIdent
    toWidget [cassius|
        .pager-buttons
            border: ridge
            padding: 3px
            text-align: left
        .pager-text
            padding-left: 1%
            padding-right: 1%
        .pager-input
            width: 40px
            text-align: right
        .pager-container
            width: 1280px
        .row-selected
            background-color: lightgrey
        .pager-header
            font-size: 20px
        th
            color: yellow
        |]
    [whamlet|
        <div ##{pgr} .pager>
            <header .pager-header>
            <div .pager-buttons>
                $if mbSelId /= Nothing
                    <button type="button" onclick="selectAndHidePager()">_{MsgSelect}
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
            var __pager_sel_src_id;
            function pRowSel(row) {
                $(".row-selected", $(#{pgrJs})).removeClass("row-selected");
                row.className += " row-selected";
                $('#' + __pager_sel_src_id + "_id").attr("_value", $(".entity-key", row).text());
                $('#' + __pager_sel_src_id).attr("_value", $(".entity-val", $(row)).text());
            }

            function pRowDblClk(row) {
                pRowSel(row);
                if (#{toJSON $ mbSelId == Nothing})
                    window.location = $("a", row).attr('href');
                else 
                    selectAndHidePager();
            }

            function showPager(isSel, hdr, addr, cnt, idRec, idAttr) {
                if (!!event)
                    event.stopPropagation();
                __pager_sel_src_id = idAttr;
                var pgr$ = $(#{pgrJs})
                    , bFst  = $('.first-btn', pgr$)[0]
                    , bLst  = $('.last-btn' , pgr$)[0]
                    , bPrev = $('.prev-btn' , pgr$)[0]
                    , bNext = $('.next-btn' , pgr$)[0]
                    , ePn   = $('.pgnum'    , pgr$)[0]
                    , ePc   = $('.pgcnt'    , pgr$)[0]
                    , content$  = $('.pager-content', pgr$)
                    , ps    = localStorage["pageSize"]
                    , isFst = true;
                ps = !!ps ? ps : '20';
                $('.reccnt', pgr$).text(cnt);
                $('.pager-header', pgr$).text(hdr);

                function loadData() {
                    var ps=pageSize();
                    var pcnt = Math.ceil(cnt / ps);
                    localStorage["pageSize"] = ps;
                    if (ePc.innerText) ePc.innerText = pcnt;
                    else ePc.innerHTML = pcnt;
                    var cp=parseInt(ePn.value);
                    ePn.value = cp = cp < 0 ? pcnt : Math.min(pcnt, Math.max(1, cp));

                    content$.load(addr, {lim: ps, off: cp <= 0 ? 0 : (cp-1)*ps, isSel: isSel, cnt: cnt, idRec: idRec, isFst: isFst});

                    isFst = false;
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
                var containerId = #{toJSON $ fromMaybe "" mbSelId};
                $(#{toJSON $ "#" <> fromMaybe "" mbSelId}).show();
                return pgr$;
            }

            function selectAndHidePager() {
                var key$ = $('#' + __pager_sel_src_id + "_id")
                    , val$ = $('#' + __pager_sel_src_id);

                key$.val(key$.attr("_value"));
                val$.val(val$.attr("_value"));

                hidePager();
            }
            function hidePager() {
                $(#{toJSON $ "#" <> fromMaybe "" mbSelId}).hide();
            }
            |]
