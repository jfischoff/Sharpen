{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module MaxEnt.Deconvolution.Internal where
import MaxEnt
import Data.List.Split
import Data.List
import Debug.Trace
import Numeric.Integration.TanhSinh
import Numeric.AD
import Control.Applicative
import MaxEnt.Deconvolution.Convolution2D
import Math.GaussianQuadratureIntegration
import MaxEnt.Deconvolution.GaussianPSF
import Debug.Trace
import Data.Word
import Data.Array.Repa hiding ((++), map, transpose)
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import qualified Data.Array.Repa.Repr.ForeignPtr as R
import qualified Data.Vector.Unboxed as UV
import Control.Lens hiding (ala)
import System.FilePath.Lens
import qualified Data.Vector.Storable as VS 
import Foreign.Marshal.Array
import System.IO.Unsafe
import Foreign.ForeignPtr.Safe
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Codec.Picture.Png
import Codec.Picture.Types
import Numeric
import Unsafe.Coerce

-- think of the point spread function as answering the question
-- how much light does the a dot at pi give to a pixel at Ilambda?

-- TODO
-- This is basically working
-- and now for some intersting stuff
-- I think that first and foremost I need to get rid of 
-- ad if I can 
-- it is to much of a pain in the ass
-- If there is a good way to get the 

traceIt x = trace (show x) x

traceItNote :: Show a => String -> a -> a
traceItNote msg x = trace (msg ++ show x) x

gaussianConvolve2D :: Double -> [[Double]] -> [[Double]]
gaussianConvolve2D var xs = 
    convolve2DZeroBoundary (sampleGaussian2D enumFromThenTo var width height) $ xs where
        width = length . head $ xs
        height = length xs

makeShow :: Show b => a -> b
makeShow = unsafeCoerce

deconvolve2D :: (forall a. RealFloat a => [[a]]) -> [[Double]] -> Either String [[Double]]
deconvolve2D psf image = result where
    convoPSF :: RealFloat a => [[a]]
    convoPSF = toConvolve2DZeroBoundary 0 psf 
    
    total = sum . concat $ image
    width = length . head $ image

    normalizedImage :: RealFloat a => [a]
    normalizedImage  =  map (fromRational . toRational) . normalize . concat $ image
    
    fromLinearImage = chunksOf width . map (total*)
    
    -- I think here I can 
    result = case linear 0.0009 (LinearConstraints convoPSF normalizedImage) of
        Right x -> Right $ fromLinearImage x
        Left x  -> Left $ show x

testPSF :: (RealFloat a) 
        => [[a]]
testPSF = map (map (fromRational . toRational)) $ 
    gaussianPSF realFloatEnum 0.5 5 5

gsamples :: (RealFloat a) 
          => (a -> a -> a -> [a]) 
          -> a -> Int -> Int -> [[a]]
gsamples fromThenTo var width height = gaussianPSF fromThenTo var width height

testInput :: [[Double]]
testInput = [[0,0,0], [0.0, 1.0, 0.0], [0,0,0]]

testInput1 :: [[Double]]
testInput1 = [[0,  0,   0, 0,   0], 
              [0,  1.0/9.0,  1.0/9.0, 1.0/9.0,   0], 
              [0,  1.0/9.0,  1.0/9.0, 1.0/9.0,   0],
              [0,  1.0/9.0,  1.0/9.0, 1.0/9.0,   0],
              [0,  0,   0, 0,   0]]


testInput2 :: [[Double]]
testInput2 = [[0,  0, 0,   0,   0], 
              [0,  0, 0,   0,   0], 
              [0,  0, 1.0, 0,   0],
              [0,  0, 0,   0,   0],
              [0,  0, 0,   0,   0]]
              
              
testInput3 :: [[Double]]
testInput3 = map (map (1.0-)) testInput2              

testDecon :: Either String [[Double]]
testDecon = deconvolve2D testPSF $ 
    (gaussianConvolve2D  0.25 testInput2)


roundTrip :: Double -> [[Double]] -> Either String [[Double]]
roundTrip var image = deconvolve2D (gsamples realFloatEnum (fromRational . toRational $ var) width height) $ 
    (gaussianConvolve2D var image) where
        width = length . head $ image
        height = length image
        
printRT var = either show (show . FloatList) . roundTrip var          


    
toWord8 :: Double -> Word8
toWord8 x = floor $ x * 256.0 


testId ::  (forall a. RealFloat a => [[a]]) -> [[Double]] -> Either String [[Double]]
testId x y = Right y    
    

                    
realFloatEnum :: RealFloat a => a -> a -> a -> [a]
realFloatEnum x y z = map (fromRational . toRational) $
    enumFromThenTo (fromRational . toRational $ x :: Double) 
                   (fromRational . toRational $ y) 
                   (fromRational . toRational $ z)


getR (PixelRGBA8 x _ _ _) = x

toPixel x = [x, x, x, 255]


--TODO make an image that matches the one above
--make increasingly larger examples until I can find the smallest ont
--that exhibits the problem
deconvolve' :: (forall a . RealFloat a => (a, (a -> a -> a -> [a])))
            -> Image Pixel8 -> Image Pixel8
deconvolve' (var, fromThenTo) i@(Image width height dat) = result where

    input :: [[Double]]
    input = [[fromIntegral (pixelAt i w h) / 256.0 | 
        w <- [0..(width  - 1)]] | 
        h <- [0..(height - 1)]  ]
        
    result = case deconvolve2D (map (map (fromRational . toRational)) $ 
                      gsamples fromThenTo var width height) $ 
                        input of
        Right z -> Image width height $ VS.fromList $ concatMap (map toWord8) z 
        Left x  -> error $ show x

-- The test that I should have, is
-- open a file and blur it
-- and then unblur it
blurImage :: Double -> Image Pixel8 -> Image Pixel8
blurImage var i@(Image width height dat) = result where
    input :: [[Double]]
    input = [[fromIntegral (pixelAt i w h) / 256.0 | 
        w <- [0..(width  - 1)]] | 
        h <- [0..(height - 1)]  ]
        
    blured = gaussianConvolve2D var input
        
    result = Image width height $ VS.fromList $ reverse $ concatMap (map toWord8) $ transpose blured

deconvolve :: Double -> FilePath -> IO ()
deconvolve var filePath = do 
    let outputPath = basename %~ (++ "_decon") $ filePath
    pngBytes <- BS.readFile filePath 
    let image = either error id . decodePng $ pngBytes
        newImage = case image of
            ImageY8 x -> deconvolve' (fromRational . toRational $ var, realFloatEnum) x
            x -> error $ "bad format"
    writePng outputPath newImage
 
deconvolve2 ::     Double -> FilePath -> IO ()
deconvolve2 var filePath = do 
    let outputPath = basename %~ (++ "_decon") $ filePath
        blurPath   = basename %~ (++ "_blur") $ filePath
    pngBytes <- BS.readFile filePath 
    let image = either error id . decodePng $ pngBytes
    
    newImage <- case image of
        ImageY8 x -> do
            --blur the image and write out the blured one
            let blurImaged = blurImage var x
            
            writePng blurPath blurImaged
            return $ deconvolve' (fromRational . toRational $ var, realFloatEnum) blurImaged
        x -> error $ "bad format"
    writePng outputPath newImage
 
 
 
 
 