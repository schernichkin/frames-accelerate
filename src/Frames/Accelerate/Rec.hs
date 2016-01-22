{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Records support in Accelerate expressions.
module Frames.Accelerate.Rec
  ( LiftedElem
  , inCoreA
  , rlens
  , rget
  , rput
  , rp
  ) where

import           Control.Monad.Primitive
import           Data.Array.Accelerate
import           Data.Array.Accelerate.Array.Sugar
import           Data.Array.Accelerate.Smart
import           Data.Array.Accelerate.Tuple
import           Data.Array.Accelerate.Type
import           Data.Typeable
import           Data.Vinyl ( HList )
import           Data.Vinyl.TypeLevel ( RIndex )
import           Frames hiding ( rlens, rget, rput )
import           Frames.InCore
import           Frames.Accelerate.HList
import           GHC.TypeLits
import           Language.Haskell.TH.Quote
import           Pipes ( Producer )
import qualified Pipes.Prelude as P

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

type LiftedElem r rs = ( RElem (Exp r) (LiftRow rs) (RIndex (Exp r) (LiftRow rs))
                       , HList rs ~ Plain (HList (LiftRow rs))
                       , Unlift Exp (HList (LiftRow rs)) )

-- | Lens to a record field
rlens :: ( Functor f
         , Elt a
         , LiftedElem (s :-> a) rs
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

-- | Getter for a record field.
rget :: ( forall f. Functor f => (a -> f a) -> Exp (HList rs) -> f (Exp (HList rs)) )
     -> Exp (HList rs) -> a
rget = hget

-- | Setter for a record field.
rput :: ( forall f. Functor f => (a -> f a) -> Exp (HList rs) -> f (Exp (HList rs)) )
     -> a -> Exp (HList rs) -> Exp (HList rs)
rput = hput

rp :: QuasiQuoter
rp = hp

-- | Stream a finite sequence of rows into an accelerate Array for further manipulation.
-- TODO: достать из VectorMs вектора и сделать из них массив для Acccelerate
inCoreA :: forall m rs . ( Applicative m, PrimMonad m, RecVec rs )
        => Producer (Record rs) m () -> m (Vector (Record rs))
inCoreA xs = do
  mvs <- allocRec (Proxy :: Proxy rs)
  let feed (!i, !sz, !mvs') row
       | i == sz = growRec (Proxy::Proxy rs) mvs'
                   >>= flip feed row . (i, sz*2,)
       | otherwise = do
          writeRec (Proxy::Proxy rs) i mvs' row
          return (i+1, sz, mvs')
      fin (n,_,mvs') = do
        vs <- freezeRec (Proxy::Proxy rs) n mvs'
        -- return . (n,) $ produceRec (Proxy::Proxy rs) vs
        return $ error "inCoreA: not implemented"
  P.foldM feed (return (0,initialCapacity,mvs)) fin xs
