{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.HUnit
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Numeric.MaxEnt.Deconvolution
import Test.QuickCheck hiding ((><))
import qualified Data.Packed.Vector as V
import qualified Data.Packed.Matrix as M
import Numeric.Container 
import Control.Applicative
import Debug.Trace
