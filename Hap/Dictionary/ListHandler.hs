{-# LANGUAGE ScopedTypeVariables, RecordWildCards, MultiParamTypeClasses #-}
module Hap.Dictionary.ListHandler where

import Hap.Dictionary.Import
import Control.Monad(liftM2)
import qualified Data.Text as T

import Hap.Dictionary.Types
import Hap.Dictionary.Pager
import Hap.Dictionary.Utils(getRoot, showPersistField, widgetToHtml)
import Hap.Dictionary.Hap

getListR :: (YesodHap m) => SomeDictionary m -> HandlerT m IO Html
getListR (sd@(SomeDictionary (_ :: [a])) :: SomeDictionary m) = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET"
    cnt <- fmap (Number . fromIntegral) $ runDB $ count ([] :: [Filter a])
    $logDebug $ T.pack $ "cnt = " ++ show cnt
    lstHead <- liftM2 (\mrHap mr -> mrHap $ MsgDictionary $ mr dn) getMessageRender getMessageRender
    root <- getRoot
    defaultLayout $ do
        setTitleI lstHead -- $ MsgListDic $ mr dn
        pager Nothing
        toWidget [julius|
            showPager(false, #{toJSON lstHead}, #{toJSON $ listR root sd}, #{cnt});
            |]
  where
    dn  = case getDictionary :: Dictionary m a of
        (Dictionary {..}) -> dDisplayName

postListR :: (YesodHap m) => SomeDictionary m -> HandlerT m IO Html
postListR (sd@(SomeDictionary (_ :: [a])) :: SomeDictionary m) = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "POST"

    pp <- getPagerParams

    (res :: [Entity a]) <- runDB $ selectList [] [Asc persistIdField, OffsetBy $ ppOffset pp, LimitTo $ ppLimit pp]
    recs <- mapM (\r -> fmap (\rec -> (rec, entityKey r, dShowFunc dic r)) $ entityToTexts ([]::[m]) r) res
{-
    let recs = map  (   entityKey
                    &&& dShowFunc dic
                    &&& -- zipWith () (dFields dic)
                        map (showPersistValue . snd)
                        . sortByPattern getDBName fst (dFields dic)
                        . (\(PersistMap m) -> m) . toPersistValue . entityVal
                    ) res
-}
    root <- getRoot
    widgetToHtml $ do
        [whamlet|
            <table width=100% border=1px>
                <tr>
                    $if not (ppIsSelect pp)
                        <th align=center bgcolor=blue width=20px>
                            <a href=#{editR root sd defKey}>+
                    $forall df <- ignoreLayout (dFields dic)
                    $# {- dPrimary dic : -} 
                        <th  bgcolor=blue>_{maybe (fsLabel (dfSettings df)) SomeMessage (dfShort df)}
                    <th align=center bgcolor=blue width=20px>-
                $forall (rec,key,txt) <- recs
                    $with isSelected <- showPersistField key == ppIdRec pp
                        <tr onclick=pRowSel(this) ondblclick=pRowDblClk(this);
                                 :isSelected:.row-selected>
                            $if not (ppIsSelect pp)
                                <td align=center>
                                    <a href=#{editR root sd (toPersistValue key)}>e
                            <td .entity-key hidden>#{showPersistField key}
                            <td .entity-val hidden>#{txt}
                            $forall x <- rec
                                    <td align=left>#{fromMaybe "" x}
                            <td align=center>
                                <button type=button onclick=pdel('#{editR root sd (toPersistValue key)}','#{listR root sd}')>-
            |]
  where
    dic = getDictionary :: Dictionary m a
    defKey = toPersistValue (def :: Key a)

