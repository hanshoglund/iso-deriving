{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Iso.Deriving
  ( Iso,
    Iso',
    As (..),
    As1 (..),
    As2 (..),
    Inject (..),
    Project (..),
    Isomorphic (..),
  )
where

import Control.Applicative
import Control.Category
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor ()
import Data.Kind
import Data.Profunctor (Profunctor (..))
import Prelude hiding ((.), id)

type Iso s t a b =
  forall p f.
  (Profunctor p, Functor f) =>
  p a (f b) ->
  p s (f t)

type Iso' s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

-- |
-- @As a b@ is represented at runtime as @b@, but we know we can in fact
-- convert it into an @a@ with no loss of information.
--
-- We can think of it as
-- having a /dual representation/ as either @a@ or @b@.
--
newtype As (a :: Type) b = As b
-- type As1 :: Type -> Type -> Type

-- |
-- Like @As@ for kind @k -> Type@.
--
newtype As1 (f :: k1 -> Type) (g :: k1 -> Type) (a :: k1)
  = As1 {getAs1 :: g a}
-- type As1 :: (k1 -> Type) -> (k1 -> Type) -> k1 -> Type

-- |
-- Like @As@ for kind @k1 -> k2 -> Type@.
--
newtype As2 f g a b
  = As2 (g a b)
-- type As2 :: (k1 -> k2 -> Type) -> (k1 -> k2 -> Type) -> k1 -> k2 -> Type

class Inject a b where
  inj :: a -> b

class Project a b where
  prj :: b -> a

-- |
-- Class of isomorphic types.
--
-- ==== Laws
--
-- [/right-inverse/]
--
--    @'inj' . 'prj' = id@
--
-- [/left-inverse/]
--
--    @'prj' . 'inj' = id@
--
-- [/compatibility/]
--
--    @'isom' = 'dimap' 'inj' ('fmap' 'prj')@
class (Inject a b, Project a b) => Isomorphic a b where
  isom :: Iso' a b
  isom = iso (inj @a @b) (prj @a @b)

instance (Project a b, Eq a) => Eq (As a b) where
  As a == As b = prj @a @b a == prj b

instance (Project a b, Ord a) => Ord (As a b) where
  compare (As a) (As b) = prj @a @b a `compare` prj b

instance (Project a b, Show a) => Show (As a b) where
  showsPrec n (As a) = showsPrec n $ prj @a @b a

instance (Isomorphic a b, Num a) => Num (As a b) where

  (As a) + (As b) =
    As $ inj @a @b $ (prj a) + (prj b)

  (As a) - (As b) =
    As $ inj @a @b $ (prj a) - (prj b)

  (As a) * (As b) =
    As $ inj @a @b $ (prj a) * (prj b)

  signum (As a) =
    As $ inj @a @b $ signum (prj a)

  abs (As a) =
    As $ inj @a @b $ abs (prj a)

  fromInteger x =
    As $ inj @a @b $ fromInteger x

instance (Isomorphic a b, Real a) => Real (As a b) where
  toRational (As x) = toRational $ prj @a @b x

instance (Isomorphic a b, Semigroup a) => Semigroup (As a b) where
  As a <> As b = As $ inj @a @b $ prj a <> prj b

instance (Isomorphic a b, Monoid a) => Monoid (As a b) where
  mempty = As $ inj @a @b mempty

instance
  (forall x. Isomorphic (f x) (g x), Functor f) =>
  Functor (As1 f g)
  where
  fmap h (As1 x) = As1 $ inj $ fmap h $ prj @(f _) @(g _) x

instance
  (forall x. Isomorphic (f x) (g x), Applicative f) =>
  Applicative (As1 f g)
  where

  pure :: forall a. a -> As1 f g a
  pure x =
    As1 $ inj @(f _) @(g _) $
      pure x

  (<*>) ::
    forall a b.
    As1 f g (a -> b) ->
    As1 f g a ->
    As1 f g b
  As1 h <*> As1 x =
    As1 $ inj @(f b) @(g b) $
      (prj @(f (a -> b)) @(g (a -> b)) h) <*> (prj @(f a) @(g a) x)

  liftA2 ::
    forall a b c.
    (a -> b -> c) ->
    As1 f g a ->
    As1 f g b ->
    As1 f g c
  liftA2 h (As1 x) (As1 y) = As1 $ inj @(f c) @(g c) $ liftA2 h (prj x) (prj y)

instance
  (forall x. Isomorphic (f x) (g x), Alternative f) =>
  Alternative (As1 f g)
  where

  empty :: forall a. As1 f g a
  empty = As1 $ inj @(f a) @(g a) $ empty

  (<|>) :: forall a. As1 f g a -> As1 f g a -> As1 f g a
  As1 h <|> As1 x =
    As1 $ inj @(f a) @(g a) $
      (prj @(f a) @(g a) h) <|> (prj @(f a) @(g a) x)

instance (forall x. Isomorphic (f x) (g x), Monad f) => Monad (As1 f g) where
  (>>=) ::
    forall a b.
    As1 f g a ->
    (a -> As1 f g b) ->
    As1 f g b
  As1 k >>= f =
    As1 $ inj @(f b) @(g b) $
      (prj @(f a) @(g a) k) >>= prj . getAs1 . f

instance
  forall f g s.
  (forall x. Isomorphic (f x) (g x), MonadState s f) =>
  MonadState s (As1 f g)
  where
  state :: forall a. (s -> (a, s)) -> As1 f g a
  state k =
    As1 $
      inj @(f a) @(g a)
        (state @s @f k)

instance
  forall f g s.
  (forall x. Isomorphic (f x) (g x), MonadReader s f) =>
  MonadReader s (As1 f g)
  where

  reader :: forall a. (s -> a) -> As1 f g a
  reader k =
    As1 $
      inj @(f a) @(g a)
        (reader @s @f k)

  local ::
    forall a.
    (s -> s) ->
    As1 f g a ->
    As1 f g a
  local f (As1 k) =
    As1 $
      inj @(f a) @(g a)
        (local f (prj @(f a) @(g a) k))

instance
  forall f g s.
  (forall x. Isomorphic (f x) (g x), MonadWriter s f) =>
  MonadWriter s (As1 f g)
  where

  writer :: forall a. (a, s) -> As1 f g a
  writer k =
    As1
      $ inj @(f a) @(g a)
      $ (writer @s @f k)

  listen ::
    forall a.
    As1 f g a ->
    As1 f g (a, s)
  listen (As1 k) =
    As1
      $ inj @(f (a, s)) @(g (a, s))
      $ listen (prj @(f a) @(g a) k)

  pass ::
    forall a.
    As1 f g (a, s -> s) ->
    As1 f g a
  pass (As1 k) =
    As1
      $ inj @(f a) @(g a)
      $ pass (prj @(f _) @(g _) k)

instance
  (forall x y. Isomorphic (f x y) (g x y), Category f) =>
  Category (As2 f g)
  where

  id :: forall a. As2 f g a a
  id =
    As2 $ inj @(f a a) @(g a a) $
      Control.Category.id @_ @a

  (.) :: forall a b c. As2 f g b c -> As2 f g a b -> As2 f g a c
  As2 f . As2 g =
    As2 $ inj @(f a c) @(g a c) $
      (Control.Category..)
        (prj @(f b c) @(g b c) f)
        (prj @(f a b) @(g a b) g)
