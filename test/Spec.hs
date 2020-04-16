{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Iso.Deriving
import Data.Monoid (Ap(..), Any(..))
import Data.Coerce (coerce)
import Control.Monad.Writer (WriterT(..))

main = pure () -- TODO

data Point a = Point { x :: a, y :: a }
  deriving (Eq, Show, Functor)

  deriving Num
    via (Squared a `As` Point a)

  deriving (Applicative, Monad)
    via (Squared `As1` Point)


type Squared = Ap ((->) Bool)

instance Isomorphic (Squared a) (Point a) where
  prj (Point x y) = coerce $ \p -> if not p then x else y
  inj x = Point (coerce x $ False) (coerce x $ True)


data NoneOrMore
  = None
    -- ^ No elements
  | OneOrMore
    -- ^ At least one element
  deriving (Semigroup, Monoid)
    via (Any `As` NoneOrMore)

instance Isomorphic Any NoneOrMore where
  inj (Any False)    = None
  inj (Any True)     = OneOrMore
  prj None           = Any False
  prj OneOrMore      = Any True

data These a b = This a | That b | These a b
  deriving stock (Functor)
  deriving (Applicative, Monad)
    via (TheseMonad a `As1` These a)

type TheseMonad a = WriterT (Maybe a) (Either a)

instance Isomorphic (TheseMonad a b) (These a b) where
  prj (This a) = WriterT (Left a)
  prj (That b) = WriterT (Right (b, Nothing))
  prj (These a b) = WriterT (Right (b, Just a))

  inj (WriterT (Left a)) = This a
  inj (WriterT (Right (b, Nothing))) = That b
  inj (WriterT (Right (b, Just a))) = These a b
