module Main where
import MaxEnt.Deconvolution
import System.Environment

main = deconvolve 0.01 . head =<< getArgs  

--main = putStr $ printRT 0.1 testInput3 