{-
File: PPM.hs
Copyright: (C) 2019 Mahjeed Marrow
Description: Defines datatype for representing PPM images
-}

module PPM(
  PPMImage (..),
  RGB (..),
  liftRGB,
  pixelMap
)where

import qualified Data.Vector as DV

{- define polymorphic ADT to represent PPM image -}
data PPMImage a = PPMImage {width    :: Int,
                            height   :: Int,
                            maxColor :: Int,
                            pixels   :: DV.Vector a}
                            deriving (Show, Eq, Ord)

{- define polymorphic ADT to represent RGB pixel -}
newtype RGB = RGB (Int, Int, Int) deriving (Show, Eq, Ord)

{- define PPMImage as instance of Functor typeclass -}
instance Functor PPMImage where
  fmap f (PPMImage w h mC pixels) = PPMImage w h mC (fmap f pixels)

{- define PPMImage as instance of Applicative typeclass -}
instance Applicative PPMImage where
  pure f = PPMImage 1 1 255 (DV.fromList [f])
  (<*>) (PPMImage w h mC partial) (PPMImage w' h' mC' pixels)
                                | w' == 10 && h' == 10 = PPMImage w' h' mC' x
                                | otherwise = error "Invalid Dimensions: Must be 10x10"
                                where x = DV.map (partial DV.! 0) pixels

{- defined PPMImage as an instance of Foldable typeclass -}
instance Foldable PPMImage where
  foldr f z (PPMImage w h mC pixels) = DV.foldr f z pixels

{- takes RGB pixel value and returns "raw" 3-tuple -}
liftRGB :: RGB -> (Int, Int, Int)
liftRGB (RGB (r, g, b)) = (r, g, b)

{- maps function to vector of pixels -}
pixelMap :: (Num a) => (a -> Int) -> (a,a,a) -> RGB
pixelMap f (r,g,b) = RGB (f r, f g, f b)
