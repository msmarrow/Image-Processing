{-
File: ImageProc.hs
Copyright: (C) 2019 Mahjeed Marrow
Description: Implements functions that alter PPM images
-}

module ImageProc(
    grayscale,
    autoThresholding,
    blend,
    Contour,
    pixSum,
    findContours
) where

import PPM
import Data.List as DL
import Data.Vector as DV

{- changes the PPM image into a grayscale image
by averaging the values of all three RGB components
and replacing them by that average -}
grayscale :: PPMImage RGB -> PPMImage RGB
grayscale ppm = fmap pixAverage ppm

{- takes pixel, calculates average of components,
and returns new pixel where each component is the average -}
pixAverage :: RGB -> RGB
pixAverage (RGB (r, g, b)) = RGB (avg,avg,avg)
                 where avg = (DL.sum [r,g,b]) `div` 3

{- applies a thresholding effect to a given image -}
autoThresholding :: Int -> PPMImage RGB -> PPMImage RGB
autoThresholding limit ppm = PPMImage w h mC pix
             where w         = (width ppm)
                   h         = (height ppm)
                   mC        = 255
                   gScale    = grayscale ppm
                   threshold = tIter 127 newThres limit gScale
                   pix       = DV.fromList (Prelude.map (replace' threshold) (DV.toList (pixels gScale)))
                   newThres  = newT gScale 127 (thresholdCount gScale 127)

{- replace pixels -}
replace' :: Int -> RGB -> RGB
replace' t (RGB (r,_,_)) = if r < t then RGB (0,0,0)
                                    else RGB (255,255,255)

{- iteratively calculate threshold value -}
tIter :: Int -> Int -> Int -> PPMImage RGB -> Int
tIter old new limit ppm = if (abs (old - new)) <= limit
                          then new
                          else tIter o n limit ppm
                          where o    = new
                                n    = newT ppm o (thresholdCount ppm limit)

{- calculates the number of pixels in an image
that are above and below a given threshold value -}
thresholdCount :: PPMImage RGB -> Int -> (Int,Int)
thresholdCount ppm t = (below,above)
               where belowVals = partitionLT ppm t
                     below = DV.length belowVals
                     aboveVals = partitionGT ppm t
                     above = DV.length aboveVals

{- calculates new threshold value for auto thresholding algorithm -}
newT :: PPMImage RGB -> Int -> (Int,Int) -> Int
newT ppm oldT (lower,upper) = (fgMean + bgMean) `div` 2
               where fgMean
                           | upper == 0 = 0
                           | otherwise = (DV.sum (partitionGT ppm oldT)) `div` upper
                     bgMean
                           | lower == 0 = 0
                           | otherwise = (DV.sum (partitionLT ppm oldT)) `div` lower

{- filters pixels that are greater than threshold value -}
partitionGT :: PPMImage RGB -> Int -> DV.Vector Int
partitionGT ppm t = DV.filter (\x -> x > t) (DV.map comp1 (pixels ppm))

{- filters pixels that are less than or equal to threshold value -}
partitionLT :: PPMImage RGB -> Int -> DV.Vector Int
partitionLT ppm t = DV.filter (\x -> x <= t) (DV.map comp1 (pixels ppm))

{- get values of individual components of pixel -}
comp1 :: RGB -> Int
comp1 (RGB (r,g,b)) = r

comp2 :: RGB -> Int
comp2 (RGB (r,g,b)) = g

comp3 :: RGB -> Int
comp3 (RGB (r,g,b)) = b

{- takes a list of PPMImage a's and blends them together
by adding the components of the pixel to the correesponding
pixel in the other image -}
blend :: [PPMImage RGB] -> PPMImage RGB
blend [x] = x
blend (x:y:xs)
              | (width x /= 10)  || (width y /= 10)  = error "Invalid Image Dimensions!"
              | (height x /= 10) || (height y /= 10) = error "Invalid Image Dimensions!"
              | otherwise = blend (result:xs)
                    where result    = purified <*> x <*> y
                          purified  = pure (pixSum)

{- sums each component in two pixels -}
pixSum :: RGB -> RGB -> RGB
pixSum (RGB (r1,g1,b1)) (RGB (r2,g2,b2))
                      | rSum < 255 && gSum < 255 && bSum < 255       = RGB (rSum,gSum,bSum)
                      | rSum >= 255 && gSum >= 255 && bSum >= 255    = RGB (255,255,255)
                      | rSum >= 255 && gSum >= 255                   = RGB (255,255,bSum)
                      | rSum >= 255 && bSum >= 255                   = RGB (255,gSum,255)
                      | gSum >= 255 && bSum >= 255                   = RGB (rSum,255,255)
                      | rSum >= 255                                  = RGB (255,gSum,bSum)
                      | gSum >= 255                                  = RGB (rSum,255,bSum)
                      | bSum >= 255                                  = RGB (rSum,gSum,255)
                      where rSum = r1 + r2
                            gSum = g1 + g2
                            bSum = b1 + b2

--defines type alias for list of int tuples called Contour
type Contour = [(Int,Int)]

{- uses tracing algorithm to find contours of an image -}
findContours :: PPMImage RGB -> ([Contour], PPMImage RGB)
findContours ppm = undefined
