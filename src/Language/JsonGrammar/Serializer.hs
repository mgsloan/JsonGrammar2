{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Language.JsonGrammar.Serializer (serializeValue) where

import Language.JsonGrammar.Grammar
import Language.JsonGrammar.Util

import Control.Applicative ((<$>), (<|>))
import Control.Monad ((>=>))
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V


-- | Convert a 'Grammar' to a JSON serializer.
serializeValue :: Grammar 'Val t1 t2 -> t2 -> Maybe t1
serializeValue = serializeValue' []

serializeValue' :: Path -> Grammar 'Val t1 t2 -> t2 -> Maybe t1
serializeValue' path = \case
  Id          -> return
  g1 :. g2    -> serializeValue' path g1 >=> serializeValue' path g2

  Empty       -> failPath path "empty grammar"
  g1 :<> g2   -> \x -> serializeValue' path g1 x <|> serializeValue' path g2 x

  Pure _ f    -> f
  Many g      -> manyM (serializeValue' path g)

  Literal val -> return . (val :-)

  Label _ g   -> serializeValue' path g

  Object g    -> \x -> do
    (obj, y) <- serializeProperties path g (H.empty, x)
    return (Ae.Object obj :- y)

  Array g     -> \x -> do
    (arr, y) <- serializeElements path g (V.empty, x)
    return (Ae.Array arr :- y)

  Coerce _ g -> serializeValue' path g


serializeProperties ::
  Path -> Grammar 'Obj t1 t2 -> (Ae.Object, t2) -> Maybe (Ae.Object, t1)
serializeProperties path = \case
  Id           -> return
  g1 :. g2     -> serializeProperties path g1 >=> serializeProperties path g2

  Empty        -> failPath path "empty grammar"
  g1 :<> g2    -> \objx ->
    serializeProperties path g1 objx <|> serializeProperties path g2 objx

  Pure _ f     -> \(obj, x) -> (obj, ) <$> f x
  Many g       -> manyM (serializeProperties path g)

  Property n g -> \(obj, x) -> do
    val :- y <- serializeValue' path g x
    return (H.insert n val obj, y)


serializeElements :: Path -> Grammar 'Arr t1 t2 -> (Ae.Array, t2) -> Maybe (Ae.Array, t1)
serializeElements path = \case
  Id        -> return
  g1 :. g2  -> serializeElements path g1 >=> serializeElements path g2

  Empty     -> failPath path "empty grammar"
  g1 :<> g2 -> \x -> serializeElements path g1 x <|> serializeElements path g2 x

  Pure _ f  -> \(arr, x) -> (arr, ) <$> f x
  Many g    -> manyM (serializeElements path g)

  Element g -> \(arr, x) -> do
    val :- y <- serializeValue' path g x
    return (V.snoc arr val, y)
