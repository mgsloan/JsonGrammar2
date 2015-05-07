module Language.JsonGrammar.Util where

import Control.Monad ((>=>), MonadPlus(..))
import Data.Text (Text)

manyM :: (Monad m, MonadPlus m) => (a -> m a) -> a -> m a
manyM m x = (m >=> manyM m) x `mplus` return x

type Path = [Text]

failPath :: Monad m => Path -> String -> m void
failPath path msg = fail $ msg ++ " with path = " ++ show path
