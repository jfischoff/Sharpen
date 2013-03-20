{- 
    Convolution matrices with various boundary conditions based on
    https://www.math.ethz.ch/education/bachelor/seminars/hs2010/ipip/slides.1.2.pdf
    
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Numeric.MaxEnt.Deconvolution.Convolution2D where
import Numeric.Container (multiply)
--import qualified Data.Packed.Vector as V
import qualified Data.Packed.Matrix as M
import Data.List (transpose)
import Data.Traversable
import Prelude hiding (concat, fromInteger)
import Data.Foldable
import Control.Arrow ((***))
import Numeric.MaxEnt.Deconvolution.FocusedList
import Data.List.Split (chunksOf)
import Numeric.MaxEnt.Deconvolution.Iterative

-- 
-- It would be nice to have a phantom type for the matrix that 
-- said whether it was row of column major
type Matrix a = [Vector a]
type Vector a = [a]

dimensions x = (length $ head x, length x)

--create a bunch of copies 
--where you increment the focus
--take a neighborhood

appendAndSlice xs pos = result where
    result = undefined

toConvolveZeroBoundary :: r -> Vector r -> Matrix r
toConvolveZeroBoundary zero vecList = result where
    result = rows
    -- rotate the vector 180 degrees
    
    len      = length vecList
    reversed = reverse vecList
    
    radius = len `div` 2 + 1
    
    --make a focused list
    Just rows = fmap (map (toList . neighborhood radius) . take len . shifts) . 
                stepBack (len - 1) . extendWith reversed zero $ zero
                
--This comes in as row
--but transpose it to columns  

zeroMatrix zero (n, m) = replicate n (replicate m zero) 

pprMatrix :: Matrix Double -> String
pprMatrix = show . M.fromLists

toConvolve2DZeroBoundary :: r -> Matrix r -> Matrix r
toConvolve2DZeroBoundary zero mat = result where
    result = fromBlocks' . toConvolveZeroBoundary 
                (zeroMatrix zero (dimensions innerMat)) $ innerMat
    innerMat = map (toConvolveZeroBoundary zero) $ columns
    columns = transpose mat
    
-- convert the input matrix input a column vector    
convolve2DZeroBoundary :: Matrix Double -> Matrix Double -> Matrix Double
convolve2DZeroBoundary input psf = result where
    (r, _) = dimensions input
    
    result = chunksOf r . concat . M.toLists $ 
                inputVector' `multiply` convolutionMat'
    
    inputVector    = concat input
    convolutionMat = toConvolve2DZeroBoundary 0 psf
    
    convolutionMat' = M.fromLists convolutionMat
    inputVector'    = M.fromLists [inputVector]


fromBlocks' :: Matrix (Matrix r) -> Matrix r
fromBlocks' = fromBlockColumns . map fromBlockRow

fromBlockRow :: [Matrix r] -> Matrix r
fromBlockRow = transpose . concat . map transpose

fromBlockColumns :: [Matrix r] -> Matrix r
fromBlockColumns = concat
  
    
{-    
input0 :: Matrix Double
input0 = [[0, 0, 0],
          [0, 1, 0],
          [0, 0, 0]]

psf0 :: Matrix Double
psf0 = [[0.125, 0.25, 0.125],
        [0.25 , 0.50, 0.25 ],
        [0.125, 0.25, 0.125]]
    

test0 :: Matrix Double
test0 = [[1, 0], 
         [0, 1]]
         
test1 :: Vector (Matrix Double)
test1 = [test0, test0]

test2 :: Matrix (Matrix Double)
test2 = [[test0, test0],
         [test0, test0] ]
-}


    
    
