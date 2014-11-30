{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hap.Dictionary.FieldFormI where
	
-- , ExistentialQuantification, RankNTypes, ScopedTypeVariables, RecordWildCards, LambdaCase	

import Hap.Dictionary.Types(FieldForm(..), FieldToText(..))
import Hap.Dictionary.Import
import qualified Data.Text as T
import Data.Time(Day)
import GHC.Int(Int64)

instance FieldToText m Text where
    fieldToText = return . Just

instance (RenderMessage m FormMessage) => FieldForm m e Text where
    fieldAForm _ fs ma = debugFormInput "Text" ma $ areq textField fs ma

instance (RenderMessage m FormMessage) => FieldForm m e (Maybe Text) where
    fieldAForm _ fs ma = debugFormInput "Maybe Text" ma $ aopt textField fs ma

instance FieldToText m Int64 where
    fieldToText = return . Just . T.pack . show

instance (RenderMessage m FormMessage) => FieldForm m e Int64 where
    fieldAForm _ fs ma = areq intField fs ma

instance (RenderMessage m FormMessage) => FieldForm m e (Maybe Int64) where
    fieldAForm _ fs ma = aopt intField fs ma

instance FieldToText m Int where
    fieldToText = return . Just . T.pack . show

instance (RenderMessage m FormMessage) => FieldForm m e Int where
    fieldAForm _ fs ma = areq intField fs ma

instance (RenderMessage m FormMessage) => FieldForm m e (Maybe Int) where
    fieldAForm _ fs ma = aopt intField fs ma

instance FieldToText m Integer where
    fieldToText = return . Just . T.pack . show

instance (RenderMessage m FormMessage) => FieldForm m e Integer where
    fieldAForm _ fs ma = areq intField fs ma

instance (RenderMessage m FormMessage) => FieldForm m e (Maybe Integer) where
    fieldAForm _ fs ma = aopt intField fs ma

instance FieldToText m Double where
    fieldToText = return . Just . T.pack . show

instance (RenderMessage m FormMessage) => FieldForm m e Double where
    fieldAForm _ fs ma = areq doubleField fs ma

instance (RenderMessage m FormMessage) => FieldForm m e (Maybe Double) where
    fieldAForm _ fs ma = aopt doubleField fs ma

instance (RenderMessage m FormMessage) => FieldForm m e Bool where
    fieldAForm _ fs ma = areq checkBoxField fs ma

instance (RenderMessage m FormMessage) => FieldForm m e (Maybe Bool) where
    fieldAForm _ fs ma = aopt checkBoxField fs ma

instance (RenderMessage m FormMessage) => FieldForm m e Day where
    fieldAForm _ fs ma = areq dayField fs ma

instance (RenderMessage m FormMessage) => FieldForm m e (Maybe Day) where
    fieldAForm _ fs ma = aopt dayField fs ma
