module Main where
import Numeric.MaxEnt.Deconvolution
import System.Environment

main = deconvolve 0.00001 . head =<< getArgs  

--main = putStr $ printRT 0.1 testInput3 