-- {-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}
module Hap.Dictionary.Hap where

import Hap.Dictionary.Import

data Hap = Hap

mkMessage "Hap" "Hap/Dictionary/messages" "en"

instance RenderMessage Hap FormMessage where
    renderMessage _ _ = defaultFormMessage
