{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import Control.Lens
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter
import Frames hiding ( rlens )
import Frames.Accelerate.Rec
import Frames.CSV

declareColumn "emplyeeId" ''Int
declareColumn "salary" ''Double

adjustSalary :: ( LiftedElem Salary rs ) => Exp (Record rs) -> Exp (Record rs)
adjustSalary rec =  rlens [rp|Salary|] %~ (*) 1.2 $ rec

emplyees :: Vector (Record [EmplyeeId, Salary])
emplyees = fromList (Z:.3)
  [ 1 &: 100 &: Nil
  , 2 &: 120 &: Nil
  , 2 &: 130 &: Nil ]

main :: IO ()
main = print $ run $ A.map adjustSalary (A.use emplyees)
