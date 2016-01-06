{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Allows to use Vinyl's HList in Accelerate expressions.
module Frames.Accelerate.HList () where

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Type
import Data.Typeable
import Data.Vinyl
import Data.Vinyl.Functor ( Identity (..) )
import Data.Vinyl.TypeLevel

type instance EltRepr  (HList '[]) = ()
type instance EltRepr' (HList '[]) = ()

type instance EltRepr  (HList (r ': rs)) = (EltRepr (HList rs), EltRepr' r)
type instance EltRepr' (HList (r ': rs)) = (EltRepr (HList rs), EltRepr' r)

instance Elt (HList '[]) where
  eltType = const UnitTuple
  fromElt = const ()
  toElt   = const RNil

  eltType' = const UnitTuple
  fromElt' = const ()
  toElt'   = const RNil

instance ( Elt r
         , Elt (HList rs)
         , Typeable rs
         , RecAll Identity rs Show
         ) => Elt (HList (r ': rs)) where
  eltType _ = PairTuple (eltType (undefined :: HList rs)) (eltType' (undefined :: r))
  fromElt (Identity r :& rs) = (fromElt rs, fromElt' r)
  toElt (xs, x) = (Identity $ toElt' x) :& toElt xs

  eltType' _ = PairTuple (eltType (undefined :: HList rs)) (eltType' (undefined :: r))
  fromElt' (Identity r :& rs) = (fromElt rs, fromElt' r)
  toElt' (xs, x) = (Identity $ toElt' x) :& toElt xs

instance IsTuple (HList '[]) where
  type TupleRepr (HList '[]) = ()

  fromTuple = const ()
  toTuple   = const RNil

instance IsTuple (HList rs) => IsTuple (HList (r ': rs)) where
  type TupleRepr (HList (r ': rs)) = (((),HList rs), r)

  fromTuple (Identity r :& rs) = (((),rs), r)
  toTuple (((),xs), x) = Identity x :& xs

type family PlainRows rs :: [*] where
  PlainRows '[] = '[]
  PlainRows (r ': rs) = Plain r ': PlainRows rs

instance Lift Exp (HList '[]) where
  type Plain (HList '[]) = HList (PlainRows '[])

  lift = const $ Exp $ Tuple NilTup

instance ( Elt (Plain r)
         , Lift Exp r
         , Typeable (PlainRows rs)
         , Elt (HList (PlainRows rs))
         , IsTuple (HList (PlainRows rs))
         , Lift Exp (HList rs)
         , RecAll Identity (PlainRows rs) Show
         , Plain (HList rs) ~ HList (PlainRows rs)
         ) => Lift Exp (HList (r ': rs)) where
  type Plain (HList (r ': rs)) = HList (PlainRows (r ': rs))

  lift (Identity r :& rs) = Exp $ Tuple $ NilTup `SnocTup` lift rs `SnocTup` lift r

instance Unlift Exp (HList '[]) where
  unlift = const RNil

instance ( Elt  r
         , Typeable (PlainRows rs)
         , Elt (HList (PlainRows rs))
         , IsTuple (HList (PlainRows rs))
         , Lift Exp (HList rs)
         , RecAll Identity (PlainRows rs) Show
         , Plain (HList rs) ~ HList (PlainRows rs)
         , Unlift Exp (HList rs)
         ) => Unlift Exp (HList (Exp r ': rs)) where
  unlift e = let h = Exp $ ZeroTupIdx `Prj` e
                 t = Exp $ SuccTupIdx ZeroTupIdx `Prj` e
             in Identity h :& unlift t
