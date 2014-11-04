{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hap.Dictionary.FieldFormI where
	
-- , ExistentialQuantification, RankNTypes, ScopedTypeVariables, RecordWildCards, LambdaCase	

import Hap.Dictionary.Types(FieldForm(..), FieldToText(..))
import Hap.Dictionary.Import

instance FieldToText m Text where
    fieldToText = return . Just

instance (RenderMessage m FormMessage) => FieldForm m e Text where
    fieldAForm es fs ma = debugFormInput "Text" ma $ areq textField fs ma

instance (RenderMessage m FormMessage) => FieldForm m e (Maybe Text) where
    fieldAForm es fs ma = debugFormInput "Maybe Text" ma $ aopt textField fs ma



