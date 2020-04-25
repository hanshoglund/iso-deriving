{-# OPTIONS_GHC -Werror#-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Data.Bifunctor
import Data.Kind
import Control.Monad.Writer (WriterT (..))
import Data.Coerce (coerce)
import Data.Traversable (for)
import Data.Monoid (Any (..), Ap (..))
import Control.Monad.State
import Control.Monad.Except
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Text.Read (Read(..), ReadPrec, pfail, look, readPrec_to_P, readP_to_Prec)
import Text.ParserCombinators.ReadP(many1, sepBy1)
import qualified Text.Read
import Data.List (isPrefixOf)
import Iso.Deriving

main = pure () -- TODO

data Point a = Point {x :: a, y :: a}
  deriving (Eq, Show, Functor)
  deriving
    (Num)
    via (Squared a `As` Point a)
  deriving
    (Applicative, Monad)
    via (Squared `As1` Point)

type Squared = Ap ((->) Bool)

instance Inject (Squared a) (Point a) where
  inj x = Point (coerce x $ False) (coerce x $ True)

instance Project (Squared a) (Point a) where
  prj (Point x y) = coerce $ \p -> if not p then x else y

instance Isomorphic (Squared a) (Point a)

data NoneOrMore
  = -- | No elements
    None
  | -- | At least one element
    OneOrMore
  deriving
    (Semigroup, Monoid)
    via (Any `As` NoneOrMore)

instance Inject Any NoneOrMore where
  inj (Any False) = None
  inj (Any True) = OneOrMore

instance Project Any NoneOrMore where
  prj None = Any False
  prj OneOrMore = Any True

instance Isomorphic Any NoneOrMore

data These a b = This a | That b | These a b
  deriving stock (Functor)
  deriving
    (Applicative, Monad)
    via (TheseMonad a `As1` These a)

type TheseMonad a = WriterT (Maybe a) (Either a)

instance Project (TheseMonad a b) (These a b) where
  prj (This a) = WriterT (Left a)
  prj (That b) = WriterT (Right (b, Nothing))
  prj (These a b) = WriterT (Right (b, Just a))

instance Inject (TheseMonad a b) (These a b) where
  inj (WriterT (Left a)) = This a
  inj (WriterT (Right (b, Nothing))) = That b
  inj (WriterT (Right (b, Just a))) = These a b

instance Isomorphic (TheseMonad a b) (These a b)

-- |
-- Abort is like 'State' but allow short-circuiting the computation.
data Abort s a = Abort { runAbort :: s -> (Maybe a, s) }
  deriving (Functor)
  deriving (Applicative, Monad, MonadState s) via
    (ExceptT () (State s) `As1` Abort s)

-- | Abort the computation. The current state will be retained, but no
-- result will be returned.
abort :: Abort s a
abort = Abort $ \s -> (Nothing, s)

quit :: a -> Abort s a
quit x = Abort $ \s -> (Just x, s)

instance Inject (ExceptT () (State s) a) (Abort s a) where
  inj (ExceptT f) = Abort $ \s -> first eitherToMaybe $ runState f s
instance Project (ExceptT () (State s) a) (Abort s a) where
  prj (Abort f) = ExceptT $ StateT $ fmap (pure . first maybeToEither) f
instance Isomorphic (ExceptT () (State s) a) (Abort s a)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing = Left ()
maybeToEither (Just x) = Right x

t :: Abort Int ()
t = do
  !x <- get
  when (x > 10) abort
  put $ x + 1
  t


data Syntax where
  Keyword :: Symbol -> Syntax
  Many1   :: Syntax -> Syntax
  SepBy1  :: Syntax -> Syntax -> Syntax
  PrimNat :: Syntax
  Then    :: Syntax -> Syntax -> Syntax


data Grammar :: Syntax -> Type where
  K :: KnownSymbol s => Grammar (Keyword s)
  M :: Grammar a -> Grammar (Many1 a)
  S :: Grammar a -> Grammar sep -> Grammar (SepBy1 a sep)
  N :: Grammar PrimNat
  T :: Grammar a -> Grammar b -> Grammar (Then a b)

class SGrammar s where
  sing :: Grammar s
instance KnownSymbol s => SGrammar (Keyword s) where
  sing = K
instance SGrammar a => SGrammar (Many1 a) where
  sing = M sing
instance (SGrammar a, SGrammar sep) => SGrammar (SepBy1 a sep) where
  sing = S sing sing
instance SGrammar PrimNat where
  sing = N
instance (SGrammar a, SGrammar b) => SGrammar (Then a b) where
  sing = T sing sing


data Parse :: Syntax -> Type where
  PK :: KnownSymbol s => Parse (Keyword s)
  PM :: [Parse a] -> Parse (Many1 a)
  PS :: [Parse a] -> Parse (SepBy1 a sep)
  PN :: Int -> Parse PrimNat
  PT :: Parse a -> Parse b -> Parse (Then a b)

showPK :: forall proxy s . KnownSymbol s => proxy (Keyword s) -> String
showPK _ = symbolVal (Proxy @s)

foo :: Grammar s -> ReadPrec (Parse s)
foo x = case x of
    k@K -> const PK <$> do
      let kw = showPK k
      la <- look
      if kw `isPrefixOf` la
        then replicateM (length kw) Text.Read.get
        else pfail
    M x -> PM <$> (readP_to_Prec $ const $ many1 $ ($ 0) $ readPrec_to_P $ foo x)
    S x sep -> PS <$> (readP_to_Prec $ const $ sepBy1
      (($ 0) $ readPrec_to_P $ foo x)
      (($ 0) $ readPrec_to_P $ foo sep)
      )
    N -> PN <$> readPrec
    T x y -> PT <$> foo x <*> foo y

instance SGrammar s => Read (Parse s) where
  readPrec = case (sing :: Grammar s) of
    g -> foo g
instance Show (Parse s) where
  show x@PK = showPK x
  show (PM xs) = concatMap show xs
  show (PS xs) = concatMap show xs -- TODO need to keep separator in parse tree for PP!
  show (PN x) = show x
  show (PT x y) = show x ++ show y

type L = (Then (Keyword "{") (Then (SepBy1 PrimNat (Keyword ",")) (Keyword "}")))

ex :: Grammar L
ex =
  T K (T (S N K) K)

ex1 :: Parse L
ex1 =
  PT PK (PT (PS [PN 1, PN 2, PN 3]) PK)

newtype Foo = Foo [Int] -- Our "AST"
  deriving (Read, Show) via (Parse L `As` Foo)

instance Inject (Parse L) Foo where
  inj (PT PK (PT (PS a) PK)) = Foo $ fmap (\(PN x) -> x) a
instance Project (Parse L) Foo where
  prj (Foo xs) = PT PK $ PT (PS $ fmap PN xs) PK
instance Isomorphic (Parse L) Foo

type L2 = PrimNat

data Foo2 = Foo2
  deriving (Show)
  deriving (Read) via (Parse L2 `As` Foo2)

instance Inject (Parse L2) Foo2 where
  inj _ = Foo2
instance Project (Parse L2) Foo2 where
  prj = undefined
instance Isomorphic (Parse L2) Foo2

{-
s :: Syntax (((), [(Int, ())]), ()) -- Isomorphic to : [Int]
s = Then (Then (Keyword "[") (Many1 $ Then PrimNat $ Keyword ",")) (Keyword "]")

-- with suitable Read/Show for Syntax, we can derive Read/Show
-}




