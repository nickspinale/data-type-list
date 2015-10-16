{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.List
    ( (:++:)
    ) where

import GHC.TypeLits
import Data.Type.Peano

type family (xs :: [a]) :++: (ys :: [a]) :: [a] where
    '[] :++: ys = ys
    (x ': xs) :++: ys = x ': (xs :++: ys)

infixr 5 :++:

type family Length (xs :: [a]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

type family Map (f :: a -> b) (xs :: [a]) :: [b] where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs

type family Foldl (f :: b -> a -> b) (zero :: b) (xs :: [a]) :: b where
    Foldl f zero '[] = zero
    Foldl f zero (x ': xs) = Foldl f (f zero x) xs

type family Foldl1 (f :: a -> a -> a) (xs :: [a]) :: a where
    Foldl1 f (x ': xs) = Foldl f x xs

type family Foldr (f :: a -> b -> b) (zero :: b) (xs :: [a]) :: b where
    Foldr f zero '[] = zero
    Foldr f zero (x ': xs) = f x (Foldr f zero xs)

type family Foldr1 (f :: a -> a -> a) (xs :: [a]) :: a where
    Foldr1 f '[x] = x
    Foldr1 f (x ': xs) = f x (Foldr1 f xs)

type family Sum (xs :: [Nat]) :: Nat where
    Sum '[] = 0
    Sum (x ': xs) = x + Sum xs

type family Replicate (n :: Peano) (x :: a) :: [a] where
    Replicate Z x = '[]
    Replicate (S n) x = x ': Replicate n x
