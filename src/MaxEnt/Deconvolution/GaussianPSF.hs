module MaxEnt.Deconvolution.GaussianPSF where
import MaxEnt
import Data.List.Split
import Debug.Trace
import Numeric.Integration.TanhSinh
import Numeric.AD
import Control.Applicative
import MaxEnt.Deconvolution.Convolution2D
import Math.GaussianQuadratureIntegration
import Numeric
import Data.List

type Coord2D a = (a, a)

normalize :: Floating a => [a] -> [a] 
normalize xs = let total = sum xs in map (/total) xs

gaussian1D :: Floating a => a -> a -> a -> a
gaussian1D variance mu x = (1.0 / (2 * pi * variance) ** 0.5) * exp (negate ((x - mu)^2) / (2 * variance))

gaussian2D :: Floating a => a -> Coord2D a -> a
gaussian2D variance (x, y) = 
    (1.0 / (2.0 * pi * variance)) * exp (negate ((x^2 + y^2) / 
        (2.0 * variance)))

doubleIntegral :: Floating a => (Coord2D a -> a) 
               -> Coord2D a 
               -> Coord2D a
               -> a
doubleIntegral f (startx, starty) (endx, endy) = nIntegrate128 ( 
      \x -> nIntegrate128 (\y -> f (x, y)) starty endy) startx endx

computeSamples2D :: (Floating a) 
               => (a -> a -> a -> [a])
               -> (Coord2D a -> a) 
               -> Coord2D a 
               -> Coord2D a
               -> Int 
               -> Int 
               -> [[a]]
computeSamples2D fromThenTo f (startx, starty) (endx, endy) widthSeg heightSeg = 
    [ [doubleIntegral f (px, py) (px + deltax, py + deltay)
                | px <- fromThenTo startx (startx + deltax) (endx - deltax)] 
                | py <- fromThenTo starty (starty + deltay) (endy - deltay)] where
        deltax = (endx - startx) / fromIntegral widthSeg
        deltay = (endy - starty) / fromIntegral heightSeg

sampleGaussian2D :: (Floating a) 
                  => (a -> a -> a -> [a])
                  -> a -> Int -> Int -> [[a]]        
sampleGaussian2D fromThenTo variance widthSeg heightSeg = 
    computeSamples2D fromThenTo (gaussian2D variance) 
        (negate extent,negate extent) (extent, extent) widthSeg heightSeg where
            extent = 1.0 --3 * (variance ** 0.5)

extendWithDefault1D :: [a] -> a -> Int -> a
extendWithDefault1D xs x i | i < 0 || length xs <= i = x
                           | otherwise =  xs !! i


newtype FloatList = FloatList [[Double]]

printRow :: [Double] -> String
printRow xs = "[" ++ 
   concat (intersperse "," (map (\x -> showFFloat (Just 6) x "") xs)) ++ "]"

instance Show FloatList where
   show (FloatList xs) = "[" ++ 
       concat (intersperse "," $ map printRow xs) ++ "]"


gaussianPSF :: (Floating a) 
            => (a -> a -> a -> [a])
            -> a -> Int -> Int -> [[a]]
gaussianPSF fromThenTo variance wSeg hSeg = 
    chunksOf wSeg . normalize . concat . 
        sampleGaussian2D fromThenTo variance wSeg $ hSeg

toCoord width i = (i `mod` width, i `div` width)



