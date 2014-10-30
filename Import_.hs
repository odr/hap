module Import_
    ( module Import_
    ) where

import          Prelude                 as Import_ hiding (head, init, last,
                                                     readFile, tail, writeFile)
import          Yesod                   as Import_ hiding (Route (..))
import          Database.Persist.Sql    as Import_ (SqlBackend)

import          Control.Applicative     as Import_ (pure, (<$>), (<*>))
import          Control.Monad           as Import_ (foldM, (>=>))
import 			Control.Arrow			as Import_ ((&&&), (***), second, first)
import          Data.Text               as Import_ (Text)

import          Data.Monoid             as Import_ (Monoid (mappend, mempty, mconcat),
                                                 (<>))

-- import Data.Maybe                       as Import_ (listToMaybe, fromMaybe, catMaybes)
-- import Data.Typeable                    as Import_
import Data.Function                    as Import_(on)
import Data.Default.Generics            as Import_(Default(..))
import Data.Maybe                       as Import_(fromMaybe)


-- import Safe                             as Import_(readMay)

-- import Data.Char                        as Import_(toLower, toUpper)

-- import Control.Monad.IO.Class           as Import_(MonadIO(..))

-- import DicTypes                     as Import_

infixl 0 #
(#) :: a -> (a -> b) -> b
x # f = f x

