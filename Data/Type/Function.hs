{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Data.Type.Function
    ( Id
    , Const
    , (:.:)
    , Flip
    , (:$:)
    , (:&:)
    , On
    ) where

type Id (x :: b) = x

type Const (x :: a) (y :: b) = x

type ((f :: b -> c) :.: (g :: a -> b)) (x :: a) = f (g x)

type Flip (f :: a -> b -> c) (x :: b) (y :: a) = f y x

type (f :: a -> b) :$: (x :: a) = f x

type (x :: a) :&: (f :: a -> b) = f x

type On (f :: b -> b -> c) (g :: a -> b) (x :: a) (y :: a) = f (g x) (g y)
