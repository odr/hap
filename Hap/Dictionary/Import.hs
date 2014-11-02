module Hap.Dictionary.Import
    ( module Hap.Dictionary.Import
    ) where

import Prelude                  as Hap.Dictionary.Import 
                                    hiding (head, init, last, readFile, tail, writeFile)
import Control.Applicative      as Hap.Dictionary.Import(pure, (<$>), (<*>))
import Control.Monad            as Hap.Dictionary.Import(foldM, (>=>))
import Control.Arrow            as Hap.Dictionary.Import((&&&), (***), second, first)
import Data.Text                as Hap.Dictionary.Import(Text)
import Data.Monoid              as Hap.Dictionary.Import(Monoid (mappend, mempty, mconcat), (<>))

import Yesod                    as Hap.Dictionary.Import hiding (Route (..))
import Database.Persist.Sql     as Hap.Dictionary.Import(SqlBackend)

import Data.Function            as Hap.Dictionary.Import(on)
import Data.Default.Generics    as Hap.Dictionary.Import(Default(..))
import Data.Maybe               as Hap.Dictionary.Import(fromMaybe)

infixl 0 #
(#) :: a -> (a -> b) -> b
x # f = f x

