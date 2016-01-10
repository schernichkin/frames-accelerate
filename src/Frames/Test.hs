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

getCol1 :: Exp (Record '[Col1, Col2, Col3])
        -> Exp Double
getCol1 rec = rec ^. rlens [rp|Col1|]

kkk = rget (rlens (Proxy :: Proxy  (Exp Col1))) testRec

-- test = liftHListTest ^. (  (V.rlens (Proxy :: Proxy Int)) . hlens )

testRec :: Exp (Record '[Col1, Col2, Col3])
testRec = lift ( 10 &: 20 &: 30 &: Nil :: Record '[Col1, Col2, Col3] )


test = getCol1 testRec

testHList :: Exp (HList '[Int, Double])
testHList = lift ( Identity 10 V.:&  Identity 20 V.:& RNil :: HList '[Exp Int, Exp Double] )
