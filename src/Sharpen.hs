module Main where
import Numeric.MaxEnt.Deconvolution
import System.Environment

main = do 
    [steps, variance, file] <- getArgs
    deconvolve (read steps) (read variance) file 0

--main = do 
--    let args = zip [0..] [0.00001, 0.0001 .. 0.01]
--    mapM (\(i, v) -> deconvolve 10000 v "images/big3.png" i) args

--main = putStr $ printRT 0.1 testInput3 