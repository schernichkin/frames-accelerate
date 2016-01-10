{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Frames.Test where

import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter
import Frames as F hiding (rlens, rget, rput)
import Frames.CSV
import Data.Vinyl as V hiding (rlens, rget, rput)
import Data.Vinyl.Functor as V
import Data.Vinyl.TypeLevel hiding (Z)
import Data.Proxy
import Control.Lens hiding ( Identity )
import Frames.Accelerate.Rec
import Frames.Accelerate.HList

import Prelude hiding ( fromIntegral )

declareColumn "col1" ''Double
declareColumn "col2" ''Double
declareColumn "col3" ''Int

-- TODO: нужна линза, которая позволит заглядывать напрямую в запись из её
-- лифтанутого представляния.

getCol1 :: Exp (Record '["col1" :-> Double, "col2" :-> Double, "col3" :-> Int])
        -> Exp Double
getCol1 rec =
          -- TODO: нужно написать тип для этой линзы
  let l = rlens  (Proxy :: Proxy  (Exp Col1))
      a = rec ^. l
   in a

kkk = rget (rlens (Proxy :: Proxy  (Exp Col1))) testRec

-- test = liftHListTest ^. (  (V.rlens (Proxy :: Proxy Int)) . hlens )

testRec :: Exp (Record '[Col1, Col2, Col3])
testRec = lift ( 10 &: 20 &: 30 &: Nil :: Record '[Col1, Col2, Col3] )


test = getCol1 testRec

testHList :: Exp (HList '[Int, Double])
testHList = lift ( Identity 10 V.:&  Identity 20 V.:& RNil :: HList '[Exp Int, Exp Double] )

-- kkk = testHList ^. hlens (Proxy :: Proxy  (Exp Int))
-- lll = hget (hlens (Proxy :: Proxy  (Exp Int))) testHList

--testHPut = hput (hlens (Proxy :: Proxy  (Exp Int))) 30
