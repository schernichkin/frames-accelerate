{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | HList support in Accelerate expressions.
module Frames.Accelerate.HList
  ( RowsExp
  , hlens
  , hget
  , hput
  ) where

import Control.Applicative
import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart hiding ( Const )
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

type family RowsExp rs :: [*] where
  RowsExp '[] = '[]
  RowsExp (r ': rs) = Exp r ': RowsExp rs

-- | Specialized version of @unlift@ which can be used to convert
-- expression of HList to HList of expressions without extra type annotations.
unliftHList :: ( Unlift Exp (HList (RowsExp rs))
               , HList rs ~ Plain (HList (RowsExp rs))
               )
            => Exp (HList rs) -> HList (RowsExp rs)
unliftHList = unlift

hlens :: ( Functor f
         , HList rs ~ Plain (HList (RowsExp rs))
         , Unlift Exp (HList (RowsExp rs))
         , RElem r (RowsExp rs) (RIndex r (RowsExp rs))
         )
      => sing r
      -> (r -> f r)
      -> Exp (HList rs)
      -> f (Exp (HList rs))
hlens k = elens . rlens k . ilens
  where
    elens f = fmap lift . f . unliftHList
    ilens f = fmap Identity . f . getIdentity

-- | Getter for a 'HList' field.
hget :: ( forall f. Functor f => (a -> f a) -> Exp (HList rs) -> f (Exp (HList rs)) )
     -> Exp (HList rs) -> a
hget l = getConst . l Const

-- | Setter for a 'HList' field.
hput :: ( forall f. Functor f => (a -> f a) -> Exp (HList rs) -> f (Exp (HList rs)) )
     -> a -> Exp (HList rs) -> Exp (HList rs)
hput l y = getIdentity . l (\_ -> Identity y)
