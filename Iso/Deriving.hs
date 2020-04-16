{-# LANGUAGE DerivingVia, RankNTypes, InstanceSigs, TypeOperators, TypeApplications, QuantifiedConstraints, StandaloneDeriving, KindSignatures, PolyKinds, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}


module Iso.Deriving
( As(..)
, As1(..)
, As2(..)
, Inject(..)
, Project(..)
, Isomorphic(..)
)
where

import Prelude hiding ((.), id)
-- import Control.Lens (Iso', iso, to, from, view, coerced, enum) -- TODO loose lens dep!
-- import Control.Monad.Free
-- import Data.Monoid hiding (Product)
import Control.Applicative
import Control.Category
import Data.Bifunctor ()
-- import Data.Maybe (catMaybes)
import Data.Profunctor (Profunctor(..))
-- import Control.Arrow (Kleisli(..))
-- import Control.Monad.State
-- import Data.Functor.Compose
-- import Data.Functor.Product
-- import Data.Functor.Const
-- import Data.Functor.Identity
-- import Data.Coerce (coerce)
-- import Control.Monad.Writer hiding (Product)

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

-- |
-- @As a b@ is represented at runtime as @b@, but we know we can in fact
-- convert it into an @a@ with no loss of information. We can think of it has
-- having a *dual representation* as either @a@ or @b@.
--
-- type As1 :: k -> Type -> Type
newtype As a b = As b

-- |
-- Like @As@ for kind @k -> Type@.
--
-- type As1 :: k1 -> (k2 -> Type) -> k2 -> Type
newtype As1 f g a   = As1 { getAs1 :: g a }

-- |
-- Like @As@ for kind @k1 -> k2 -> Type@.
--
-- type As2 :: k1 -> (k2 -> k3 -> Type) -> k2 -> k3 -> Type
newtype As2 f g a b = As2 (g a b)


class Inject a b where
  inj :: a -> b

class Project a b where
  prj :: b -> a

-- |
-- Laws: 'isom' is an isomorphism, that is:
--
-- @
-- view isom . view (from isom) = id = view (from isom) . view isom
-- @
class (Inject a b, Project a b) => Isomorphic a b where
  isom :: Iso' a b
  isom = iso inj prj

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

instance (Isomorphic a b, Eq a) => Eq (As a b) where
  As a == As b = prj @a @b a == prj b

instance (Isomorphic a b, Ord a) => Ord (As a b) where
  compare (As a) (As b) = prj @a @b a `compare` prj b

instance (Isomorphic a b, Semigroup a) => Semigroup (As a b) where
  As a <> As b = As $ inj @a @b $ prj a <> prj b

instance (Isomorphic a b, Monoid a) => Monoid (As a b) where
  mempty = As $ inj @a @b mempty

instance (forall x . Isomorphic (f x) (g x), Functor f) => Functor (As1 f g) where
  fmap h (As1 x) = As1 $ inj $ fmap h $ prj @(f _) @(g _) x

instance (forall x . Isomorphic (f x) (g x), Applicative f) => Applicative (As1 f g) where
  pure x = As1 $ inj @(f _) @(g _) $ pure x

  (<*>) :: forall a b . As1 f g (a -> b) -> As1 f g a -> As1 f g b
  As1 h <*> As1 x = As1 $ inj @(f b) @(g b) $ (prj @(f (a -> b)) @(g (a -> b)) h) <*> (prj @(f a) @(g a) x)

  liftA2 :: forall a b c . (a -> b -> c) -> As1 f g a -> As1 f g b -> As1 f g c
  liftA2 h (As1 x) (As1 y) = As1 $ inj @(f c) @(g c) $ liftA2 h (prj x) (prj y)

instance (forall x . Isomorphic (f x) (g x), Alternative f) => Alternative (As1 f g) where
  empty :: forall a . As1 f g a
  empty = As1 $ inj @(f a) @(g a) $ empty

  (<|>) :: forall a . As1 f g a -> As1 f g a -> As1 f g a
  As1 h <|> As1 x = As1 $ inj @(f a) @(g a) $ (prj @(f a) @(g a) h) <|> (prj @(f a) @(g a) x)

instance (forall x . Isomorphic (f x) (g x), Monad f) => Monad (As1 f g) where
  (>>=) :: forall a b . As1 f g a -> (a -> As1 f g b) -> As1 f g b
  As1 k >>= f = As1 $ inj @(f b) @(g b) $ (prj @(f a) @(g a) k) >>= prj . getAs1 . f

instance (forall x y . Isomorphic (f x y) (g x y), Category f) => Category (As2 f g) where
  id :: forall a . As2 f g a a
  id = As2 $ inj @(f _ _) @(g _ _) $ Control.Category.id @_ @a

  (.) :: forall a b c . As2 f g b c -> As2 f g a b -> As2 f g a c
  As2 f . As2 g = As2 $ inj @(f a c) @(g a c) $ (Control.Category..)
    (prj @(f b c) @(g b c) f) (prj @(f a b) @(g a b) g)


