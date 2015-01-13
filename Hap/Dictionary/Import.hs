{-# LANGUAGE TemplateHaskell, OverloadedStrings, NoImplicitPrelude #-}
module Hap.Dictionary.Import
    ( module Hap.Dictionary.Import
    ) where

import ClassyPrelude            as Hap.Dictionary.Import 
                                    -- hiding (head, init, last, readFile, tail, writeFile)
import Control.Applicative      as Hap.Dictionary.Import(Const(..))
import Data.Monoid              as Hap.Dictionary.Import(Last(..))

import Yesod                    as Hap.Dictionary.Import hiding (Route (..), parseTime)
import Database.Persist.Sql     as Hap.Dictionary.Import(SqlBackend)

import Data.Default.Generics    as Hap.Dictionary.Import(Default(..))

import qualified Data.Text.Lazy as LT
import Data.Text.Format.Params(Params)
import Data.Text.Format(Format, left, right, build)
import qualified Data.Text.Lazy.Builder as DTLB
import Data.Text.Format         as Hap.Dictionary.Import(Only(..), Shown(..))

infixl 0 #
(#) :: a -> (a -> b) -> b
x # f = f x

debugMess :: Params ps => Format -> ps -> Text
debugMess f ps 
    = LT.toStrict $ DTLB.toLazyText $ mconcat
            [ left 10 '=' (" " :: Text)
            , build f ps
            , right 10 '=' (" " :: Text)
            ]


debugFormInput 
    :: (Default b, Monoid (t m (Last b)), MonadLogger m, MonadTrans t,
        Show a, Functor (t m)) 
    => Format -> a -> t m b -> t m b
debugFormInput t mv f
    = fmap (fromMaybe def . getLast)
        $ fmap Last (lift $ do
                $logDebug $ debugMess (t ++ ": {}") $ Only $ Shown mv
                return def
            ) ++ fmap (Last . Just) f
