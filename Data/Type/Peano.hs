{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Peano
    ( Peano(..)
    ) where

import GHC.TypeLits

data Peano = Z | S Peano

type family FromPeano (n :: Peano) :: Nat where
    FromPeano Z = 0
    FromPeano (S n) = 1 + FromPeano n
