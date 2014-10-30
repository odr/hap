-- {-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}
module Hap.Dictionary.Hap where

import Import_

data Hap = Hap

mkMessage "Hap" "messages_dic" "en"

instance RenderMessage Hap FormMessage where
    renderMessage _ _ = defaultFormMessage
