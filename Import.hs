module Import
    ( module Import
    ) where

import Import_              as Import
import Foundation           as Import
import Model                as Import
import Settings             as Import
import Settings.Development as Import
import Settings.StaticFiles as Import

import Hap.Dictionary.DicTypes   as Import

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
widgetToHtml = (>>= withUrlRenderer) . fmap pageBody . widgetToPageContent

setMessageWidget :: Widget -> Handler ()
setMessageWidget = (>>=setMessage) . widgetToHtml
