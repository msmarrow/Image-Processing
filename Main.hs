{-
File: Main.hs
Copyright: (C) 2019 Mahjeed Marrow
-}

module Main(
  main
)where

import System.IO
import System.Environment
import Data.List as DL
import qualified Data.Vector as DV
import qualified Data.Traversable as DT
import qualified ImageProc as IP
import PPM

usage :: String
usage = "usage: imageproc <PPM in file path> <PPM out file path> <proc>" ++
         "<proc>:" ++
          "contour" ++
                "-- Finds the contours of the in" ++
                "file path. Prints the list of" ++
                "contours to the screen" ++
          "blend <PPM file1> ... <PPM fileN>:"++
                "-- Blends the provided images"++
                "with the <PPM in file path> image"++
                "and saves it to <PPM out file path>" ++
          "autoThreshold <LIMIT>"++
                "-- Performs an auto thresholding effect"++
                "on the <PPM in file path> image and"++
                "saves it to <PPM out in file path>"++
                "Limit is the max number of iterations"++
                "during auto-thresholding." ++
          "grayscale -- Performs a grayscale effect on the"++
                "<PPM in file path> and saves it to"++
                "<PPM out file path>"

{- gets list of words from parsed PPM file -}
words' :: String -> [String]
words' str = DL.words str

{- creates a vector of RGB pixels -}
createVector :: [String] -> [RGB]
createVector [] = []
createVector (x:y:z:xs) = makePixel [x,y,z] : createVector xs

{- takes list of strings representing ints and returns RGB pixel -}
makePixel :: [String] -> RGB
makePixel [a,b,c] = RGB (a', b', c')
                  where a' = read a :: Int
                        b' = read b :: Int
                        c' = read c :: Int

{- verifies whether a candidate PPM file is valid -}
validPPM :: [String] -> Bool
validPPM str
            | (str !! 0) !! 0 /= 'P'    = False
            | (str !! 3) /= "255"       = False
            | pixels /= width * height  = False
            | otherwise                 = True
            where pixels  = (length (drop 4 str)) `div` 3
                  width   = read (str !! 1) :: Int
                  height  = read (str !! 2) :: Int

{- handles error in case of invalid PPM image -}
errorHandler :: Bool -> String -> IO()
errorHandler False file = error ("Invalid PPM Image: " ++ show file)
errorHandler True file  = return()

{- takes PPMImage representation and converts to string -}
stringify :: PPMImage RGB -> String
stringify (PPMImage w h mC p) = "P3\n" ++ show w ++ "\t" ++ show h ++ "\n" ++ show mC ++ "\n" ++ pixels
                              where pixels = concat (DV.map pixToStr p)

{- converts pixel to list of components represented as chars -}
pixToStr :: RGB -> [Char]
pixToStr (RGB (r,g,b)) = show r ++ " " ++ show g ++ " " ++ show b ++ " \n"

ppmFromFile :: String -> PPMImage RGB
ppmFromFile str = PPMImage width height 255 (DV.fromList (parsed))
                where width   = read (words' str !! 1) :: Int
                      height  = read (words' str !! 2) :: Int
                      parsed  = createVector (drop 4 (words' str))

main :: IO()
main = do
  args <- getArgs
  let inputFile   = (args !! 0)
  let outputFile  = (args !! 1)
  --read input file and parse
  inputImage      <- readFile inputFile
  let width       = read (words' inputImage !! 1) :: Int
  let height      = read (words' inputImage !! 2) :: Int
  let parsedImg   = createVector (drop 4 (words' inputImage))
  --validate PPM image before processing
  errorHandler (validPPM (words' inputImage)) inputFile

  case (args !! 2) of
    -- RUN GRAYSCALE FUNCTION
    "grayscale" -> do
        let img = IP.grayscale $ PPMImage width height 255 (DV.fromList (parsedImg))
        writeFile outputFile (stringify img)
        return()
    -- RUN BLEND FUNCTION
    "blend" -> do
        let argList    = (drop 3 args)
        readFiles      <- DT.mapM readFile argList
        let mapping    = map ppmFromFile readFiles
        writeFile outputFile (stringify (IP.blend mapping))
        return()
    -- RUN AUTO THRESHOLD FUNCTION
    "autoThreshold" -> do
         let limit = read (args !! 3) :: Int
         let img = PPMImage width height 255 (DV.fromList (parsedImg))
         writeFile outputFile (stringify (IP.autoThresholding limit img))
         return()

    -- RUN FIND CONTOURS FUNCTION
