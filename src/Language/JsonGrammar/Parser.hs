{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Language.JsonGrammar.Parser (parseValue) where

import Language.JsonGrammar.Grammar
import Language.JsonGrammar.Util

import Control.Applicative ((<$>))
import Control.Monad ((>=>), unless)
import Data.Aeson (Object, Array, withObject, (.:), withArray)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Monoid ((<>))
import qualified Data.Vector as V

-- | Convert a 'Grammar' to a JSON 'Parser'.
parseValue :: Grammar 'Val t1 t2 -> t1 -> Parser t2
parseValue = parseValue' []

parseValue' :: Path -> Grammar 'Val t1 t2 -> t1 -> Parser t2
parseValue' path = \case
  Id        -> return
  g1 :. g2  -> parseValue' path g2 >=> parseValue' path g1
  Empty     -> \_ -> failPath path "empty grammar"
  g1 :<> g2 -> parseValue' path g1 <> parseValue' path g2
  Pure f _  -> f
  Many g    -> manyM (parseValue' path g)

  Literal val -> \(val' :- t) ->
    if val == val'
      then return t
      else typeMismatch "literal" val'

  Label l g -> parseValue' (l : path) g

  Object g -> \(val :- x) ->
    withObject "object" (\obj -> parseProperties path obj g x) val

  Array g -> \(val :- x) -> do
      (arr', y) <- withArray "array" (\arr -> parseElements path g (arr, x)) val
      unless (V.null arr') $ typeMismatch "end of array" (V.head arr')
      return y

  Coerce _ g -> parseValue' path g



parseProperties :: Path -> Object -> Grammar 'Obj t1 t2 -> t1 -> Parser t2
parseProperties path obj = \case
  Id            -> return
  g1 :. g2     -> parseProperties path obj g2 >=> parseProperties path obj g1

  Empty        -> \_ -> failPath path "empty grammar"
  g1 :<> g2    -> parseProperties path obj g1 <> parseProperties path obj g2

  Pure f _     -> f
  Many g       -> manyM (parseProperties path obj g)

  Property n g -> \x -> do
    val <- obj .: n
    parseValue' path g (val :- x)


parseElements :: Path -> Grammar 'Arr t1 t2 -> (Array, t1) -> Parser (Array, t2)
parseElements path = \case
  Id        -> return
  g1 :. g2  -> parseElements path g2 >=> parseElements path g1

  Empty     -> \_ -> failPath path "empty grammar"
  g1 :<> g2 -> parseElements path g1 <> parseElements path g2

  Pure f _  -> \(arr, x) -> (arr, ) <$> f x
  Many g    -> manyM (parseElements path g)

  Element g -> \(arr, x) ->
    if V.null arr
      then failPath path "expected at least one more array element"
      else do
        y <- parseValue' path g (V.last arr :- x)
        return (V.init arr, y)
