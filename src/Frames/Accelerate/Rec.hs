{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Allows to use Frame's records in Accelerate expressions.
module Frames.Accelerate.Rec ( ) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Type
import Data.Typeable
import Frames
import GHC.TypeLits

type instance EltRepr  (s :-> a) = ((), EltRepr' a)
type instance EltRepr' (s :-> a) = EltRepr' a

instance ( Elt a
         , Typeable s
         , KnownSymbol s
         ) => Elt (s :-> a) where
  eltType (Col a) = PairTuple UnitTuple (eltType' a)
  fromElt (Col a) = ((), fromElt' a)
  toElt   ((), a) = Col $ toElt' a

  eltType' (Col a) = eltType' a
  fromElt' (Col a) = fromElt' a
  toElt'   a       = Col $ toElt' a

instance IsTuple (s :-> a) where
  type TupleRepr (s :-> a) = ((), a)
  fromTuple (Col a) = ((), a)
  toTuple   ((), a) = Col a

instance ( Elt (Plain a)
         , Lift Exp a
         , Typeable s
         , KnownSymbol s
         ) => Lift Exp (s :-> a) where
  type Plain (s :-> a) = s :-> Plain a

  lift (Col a) = Exp $ Tuple (NilTup `SnocTup` lift a)

instance ( Elt a
         , KnownSymbol s
         , Typeable s
         ) => Unlift Exp (s :-> Exp a) where
  unlift e = let a = Exp $ ZeroTupIdx `Prj` e
              in Col a
