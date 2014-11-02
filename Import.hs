module Import
    ( module Import
    ) where

import Hap.Dictionary.Import    as Import
import Hap.Dictionary.Types     as Import
import Settings                 as Import
import Settings.Development     as Import
import Settings.StaticFiles     as Import
import Model                    as Import
import Foundation               as Import
-- import Dics                     as Import


pureLayout :: Widget -> Handler Html
pureLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer [hamlet|
        $doctype 5
        <html>
            <head>
                ^{pageHead pc}
            <body>
                ^{pageBody pc}
        |]

widgetToHtml :: Widget -> Handler Html
widgetToHtml = fmap pageBody . widgetToPageContent >=> withUrlRenderer

setMessageWidget :: Widget -> Handler ()
setMessageWidget = widgetToHtml >=> setMessage
