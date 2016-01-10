{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Records support in Accelerate expressions.
module Frames.Accelerate.Rec
  ( rlens
  , rget
  , rput
  ) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Type
import Data.Typeable
import Data.Vinyl ( HList )
import Data.Vinyl.TypeLevel ( RIndex )
import Frames hiding ( rlens, rget, rput )
import Frames.Accelerate.HList
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

rlens :: ( Functor f
         , Elt a
         , HList rs ~ Plain (HList (RowsExp rs))
         , Unlift Exp (HList (RowsExp rs))
         , RElem (Exp (s :-> a)) (RowsExp rs) (RIndex (Exp (s :-> a)) (RowsExp rs))
         , Typeable s
         , KnownSymbol s
         )
      => sing (Exp (s :-> a))
      -> (Exp a -> f (Exp a))
      -> Exp (HList rs)
      -> f (Exp (HList rs))
rlens k = hlens k . clens
  where
    clens f = fmap (lift . Col) . f . getCol . unlift

-- | Getter for a 'HList' field.
rget :: ( forall f. Functor f => (a -> f a) -> Exp (HList rs) -> f (Exp (HList rs)) )
     -> Exp (HList rs) -> a
rget = hget

-- | Setter for a 'HList' field.
rput :: ( forall f. Functor f => (a -> f a) -> Exp (HList rs) -> f (Exp (HList rs)) )
     -> a -> Exp (HList rs) -> Exp (HList rs)
rput = hput
