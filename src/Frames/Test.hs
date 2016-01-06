{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Frames.Test
    (
    ) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter
import Frames as F
import Frames.Accelerate
import Frames.CSV
import Data.Vinyl as V
import Data.Vinyl.Functor as V
import Data.Vinyl.TypeLevel hiding (Z)
import Data.Proxy
import Control.Lens hiding ( Identity )
import Data.Vinyl.Functor ( Identity (..) )

import Prelude hiding ( fromIntegral )

declareColumn "col1" ''Double
declareColumn "col2" ''Double
declareColumn "col3" ''Int

recordExp :: Exp (Record '[Col1, Col2, Col3])
recordExp = lift ( 10 &: 20 &: 30 &: Nil :: Record '[Col1, Col2, Col3] )

unlifedRecord :: Record '[Exp Col1, Exp Col2, Exp Col3]
unlifedRecord = unlift recordExp

lensToUnlifted :: ( Functor g, RElem (Exp Col1) rs (RIndex (Exp Col1) rs) ) => (f (Exp Col1) -> g (f (Exp Col1))) -> Rec f rs -> g (Rec f rs)
lensToUnlifted = V.rlens (Proxy :: Proxy (Exp Col1))

test1 :: V.Identity (Exp Col1) -- тут лучше иметь возможность достать значение col.
test1 = let (V.Identity a) = unlifedRecord ^. lensToUnlifted
            --b = (unlift a)
        in (V.Identity a)

sinleCol1Lift :: Exp Col1
sinleFieldLift = lift (Col (10.0::Double))

fieldLift :: Exp (Record '[Col1, Col2, Col3])
fieldLift = lift ( 10 &: 20 &: 30 &: Nil :: Record '[Col1, Col2, Col3] )

-- эта штука работает фактически как геттер.
fieldUnlift :: Exp Double
fieldUnlift = let u = unlift fieldLift :: Record '[Exp Col1, Exp Col2, Exp Col3]
                  (Identity k) = test15 ^. qq2
                  (Col j) = unlift k :: "field1" :-> Exp Double
              in j

-- TODO: подумать, как можно использовать записи без лифтинга/анлифтинга.
-- 1. lift/unlift требует введения дополнительных переменных.
-- 2. lift/unlift требует явного указания типов.
--    можно попробовать избавиться от анлифтинга посредством написания специальных
--    линз которые могли бы брать значения из Exp
-- 3. Стандартные линзы не подходят для lifted записей.
--    Замусоривать прастранства имён доополнительными линзами не очень хорошо,
--    нужно посмотреть на возможности TH для этого.

bb2 :: Identity (Exp Col1)
bb2 = test15 ^. qq2

qq2 :: ( Functor g, RElem (Exp Col1) rs (RIndex (Exp Col1) rs) )
    => (f (Exp Col1) -> g (f (Exp Col1))) -> Rec f rs -> g (Rec f rs)
qq2 = V.rlens (Proxy :: Proxy  (Exp Col1))
-- qq2 = V.rlens [p|Exp Col1|]

bb = test17 ^. qq

aa = test17 ^. tt

qq :: ( Functor g, RElem Col1 rs (RIndex Col1 rs) ) => (f Col1 -> g (f Col1)) -> Rec f rs -> g (Rec f rs)
qq = V.rlens (Proxy :: Proxy  Col1)

tt :: ( Functor f, RElem Col1 rs (RIndex Col1 rs) ) => (Double -> f Double) -> Record rs -> f (Record rs)
tt = F.rlens (Proxy :: Proxy  Col1)

test17 :: Record '[Col1, Col2, Col3]
test17 = 10 &: 20 &: 30 &: RNil

test16 :: Exp (Record '[Col1, Col2, Col3])
       -> Exp (Record '[Col1, Col2, Col3])
       -> Exp (Record '[Col1, Col2, Col3])
test16 a b =
  let ua = unlift a :: Record '[Exp Col1, Exp Col2, Exp Col3]
      ub = unlift b :: Record '[Exp Col1, Exp Col2, Exp Col3]
  --    с = F.rget $ "field1" ua
  in lift ( 10 &: 20 &: 30 &: Nil :: Record '[Col1, Col2, Col3] )

test15 :: Record '[Exp Col1, Exp Col2, Exp Col3]
test15 = unlift test14

test14 :: Exp (Record '[Col1, Col2, Col3])
test14 = lift ( 10 &: 20 &: 30 &: Nil :: Record '[Col1, Col2, Col3] )

test13 = run test12

test12 :: Acc (Scalar (HList '[Double, Double, Int]))
test12 = test10 test11

test11 :: Acc (Vector (HList '[Double, Double, Int]))
test11 = generate (index1 10) (\ix ->
  let (Z :. i) = unlift ix
  in lift $ Identity (fromIntegral i * 10)
       V.:& Identity (fromIntegral i * 100)
       V.:& Identity i
       V.:& RNil)

test10 :: Acc (Vector (HList '[Double, Double, Int]))
       -> Acc (Scalar (HList '[Double, Double, Int]))
test10 = fold test9 (lift $ Identity (0 :: Double)
                       V.:& Identity (0 :: Double)
                       V.:& Identity (0 :: Int)
                       V.:& RNil)

test9 :: Exp (HList '[Double, Double, Int])
      -> Exp (HList '[Double, Double, Int])
      -> Exp (HList '[Double, Double, Int])
test9 a b =
  let ua = unlift a :: HList '[Exp Double, Exp Double, Exp Int]
      ub = unlift b :: HList '[Exp Double, Exp Double, Exp Int]
   in case (ua, ub) of
     (Identity ua1 V.:& Identity ua2 V.:& Identity ua3 V.:&RNil,
      Identity ub1 V.:& Identity ub2 V.:& Identity ub3 V.:&RNil) -> lift $
        Identity (ua1 + ub1) V.:& Identity (ua2 + ub2) V.:& Identity (ua3 + ub3) V.:& RNil

test8 :: Exp (HList '[Double, Int]) -> HList '[Exp Double, Exp Int]
test8 = unlift

test7 :: HList '[]
test7 = unlift test4

test6 :: Exp (HList '[Double, Double, Int])
test6 = lift $ Identity (10 :: Double) V.:& Identity (11 :: Double) V.:& Identity (20 ::  Int) V.:& RNil

test5 :: Exp (HList '[Int])
test5 = lift $ Identity (10 :: Int) V.:& RNil

test4 :: Exp (HList '[])
test4 = lift (RNil :: Rec Identity '[])
