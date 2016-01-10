{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | HList support in Accelerate expressions.
module Frames.Accelerate.HList
  ( LiftRow
  , hlens
  , hget
  , hput
  , hp
  ) where

import Control.Applicative
import Data.Array.Accelerate hiding ( map, not )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart hiding ( Const )
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Type
import Data.Char
import Data.Typeable
import Data.Vinyl
import Data.Vinyl.Functor ( Identity (..) )
import Data.Vinyl.TypeLevel
import Language.Haskell.TH hiding ( Exp )
import Language.Haskell.TH.Quote

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

type family LiftRow rs :: [*] where
  LiftRow '[] = '[]
  LiftRow (r ': rs) = Exp r ': LiftRow rs

-- | Specialized version of @unlift@ which can be used to convert
-- expression of HList to HList of expressions without extra type annotations.
unliftHList :: ( Unlift Exp (HList (LiftRow rs))
               , HList rs ~ Plain (HList (LiftRow rs))
               )
            => Exp (HList rs) -> HList (LiftRow rs)
unliftHList = unlift

hlens :: ( Functor f
         , HList rs ~ Plain (HList (LiftRow rs))
         , Unlift Exp (HList (LiftRow rs))
         , RElem r (LiftRow rs) (RIndex r (LiftRow rs))
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

-- | Remove white space from both ends of a 'String'.
trim :: String -> String
trim = takeWhile (not . isSpace) . dropWhile isSpace

-- | Split on a delimiter.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = go
  where go [] = []
        go xs = let (h,t) = break (== d) xs
                in case t of
                     [] -> [h]
                     (_:t') -> h : go t'

-- | A proxy value quasiquoter; a way of passing types as
-- values. @[pr|T|]@ will splice an expression @Proxy::Proxy (Exp T)@, while
-- @[pr|A,B,C|]@ will splice in a value of @Proxy :: Proxy
-- [Exp A, Exp B, Exp C]@. If we have a record type with @Name@ and @Age@ among
-- other fields, we can write @select @[pr|Name,Age|]@ for a function
-- that extracts those fields from a larger record.
hp :: QuasiQuoter
hp = QuasiQuoter mkProxy undefined undefined undefined
  where mkProxy s = let ts = map trim $ splitOn ',' s
                        cons = mapM (\name -> [t| Exp $(conT $ mkName name) |]) ts
                        mkList = foldr (AppT . AppT PromotedConsT) PromotedNilT
                    in case ts of
                         [h@(t:_)]
                             | isUpper t -> [|Proxy::Proxy $(fmap head cons)|]
                             | otherwise -> [|Proxy::Proxy $(varT $ mkName h)|]
                         _ -> [|Proxy::Proxy $(fmap mkList cons)|]
