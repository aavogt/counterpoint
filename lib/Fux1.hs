{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Fux1 where

import Control.Lens hiding ((.>))
import Control.Lens.Action
import Control.Monad
import Data.Compression.Huffman
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.SBV
import Data.SBV.Dynamic hiding (allSatWith)
import Data.SBV.Internals
import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import Text.Read

-- Configuration for first species counterpoint
-- but perhaps a better way to organize this is as separate functions that just add the constraints?
data Fux1 = Fux1
  { cadenceType :: CadenceType,
    voicePosition :: VoicePosition, -- counterpoint above or below cantus firmus
    allowCrossing :: Bool, -- allow voices to cross
    allowedRange :: (Int, Int), -- MIDI range for counterpoint voice

    -- | number of repeated notes in the voice
    retriggers :: Int,
    nsol :: Int
  }
  deriving (Show, Eq)

data CadenceType = Authentic | Plagal | Half deriving (Show, Eq)

data VoicePosition = Above | Below deriving (Show, Eq)

-- Main function: generate all valid counterpoints
fux1 :: Fux1 -> [Int] -> IO [[Int]]
fux1 config cantus = do
  if null (drop 2 cantus)
    then return []
    else do
      solutions <- allSatWith defaultSMTCfg {allSatMaxModelCount = Just (nsol config)} $ do
        counterpoint <- sIntegers $ map (("cp" ++) . show) (zipWith const [0 ..] cantus)
        applyFuxConstraints config cantus counterpoint
      print solutions
      return $ extractSolutions solutions

-- Apply all first species counterpoint constraints
applyFuxConstraints :: Fux1 -> [Int] -> [SInteger] -> Symbolic ()
applyFuxConstraints config (map fromIntegral -> gs) xs = do
  let lag x = zip x (drop 1 x)

  -- absolute difference between xs and gs
  -- xg interval
  let adxgs = [abs (x - g) `sEMod` 12 | (x, g) <- zip xs gs]

  constrain $
    sum [oneIf @Integer (x .== y) | (x, y) <- lag xs]
      .<= fromIntegral (retriggers config)

  -- Range constraints
  do
    let (minNote, maxNote) = allowedRange config & each %~ fromIntegral
    constrain $ sAll (.>= minNote) xs
    constrain $ sAll (.<= maxNote) xs

  -- each simultaneous pair cannot be dissonant
  constrain $ sAll (`sNotElem` [1, 2, 6, 10, 11]) adxgs

  -- Voice crossing constraints
  case voicePosition config of
    _ | allowCrossing config -> return ()
    Above -> constrain $ sAnd $ zipWith (.>=) xs gs
    Below -> constrain $ sAnd $ zipWith (.<=) xs gs

  -- no big jumps
  constrain $ sAll (\(a, b) -> abs (a - b) .<= 12) (lag xs)

  -- no parallel fifths or octaves
  constrain $
    sAnd
      [ i ./= k .|| j ./= k
        | k <- [0, 7],
          (i, j) <- lag adxgs
      ]

  -- Do not use more than three of the same imperfect consonance type in a row (e.g., three thirds in a row).
  constrain $
    sAnd
      [ sAny (./= m) ijk
        | m <- [3, 4, 8, 9],
          ijk <- map (take 3) $ takeWhile (not . null . drop 2) $ tails adxgs
      ]

  -- start with a fifth/octave/unison
  adxgs ^!? _head . act \x -> constrain (x `sElem` [0, 7])

  -- end with a unison/octave
  adxgs ^!? _last . act \x -> constrain (x .== 0)

  case (reverse gs, reverse xs) of
    (g : _, _ : xP : _) -> constrain $ applyCadence (cadenceType config) g xP
    _ -> return ()

applyCadence :: CadenceType -> SInteger -> SInteger -> SBool
applyCadence cadenceType g xP =
  case cadenceType of
    Half -> xP .== g
    Plagal -> xP .== g + 1
    Authentic -> xP .== g - 1

-- Extract concrete solutions from SBV results
extractSolutions :: AllSatResult -> [[Int]]
extractSolutions (AllSatResult _ _ _ solutions) =
  map extractModel solutions
  where
    extractModel :: SMTResult -> [Int]
    extractModel (Satisfiable _ model) =
      let dict = modelAssocs model
          f = stripPrefix "cp" >=> readMaybe
          sorted = [(n :: Int, val) | (f -> Just n, CV _ (CInteger val)) <- dict]
       in map (fromInteger . snd) $ Data.List.sortBy (\(a, _) (b, _) -> compare a b) sorted
    extractModel _ = []

-- Helper function to create common configurations
defaultFux1 :: Fux1
defaultFux1 =
  Fux1
    { cadenceType = Plagal,
      voicePosition = Above,
      allowCrossing = False,
      retriggers = 2,
      allowedRange = (60, 72), -- Middle C to C5
      nsol = 5000
    }

-- Example usage
exampleCantus :: [Int]
exampleCantus = [60, 62, 64, 62, 57, 60] -- C-D-E-D-A-C

-- has for 4608 valid counterpoints
-- huffman
-- huffman :: (Ord w, Num w) => [(a, w)] -> HuffmanTree a
-- `a` should be an n-gram

ngram n = takeWhile ((== n) . length) . map (take n) . tails

runExample :: IO ()
runExample = do
  putStrLn "Generating counterpoints for cantus firmus: C-D-E-D-C"
  solutions <- fux1 defaultFux1 exampleCantus

  mapM_ print $ sortOn snd $ M.toList $ M.fromListWith (+) $ concat [map (,1) $ map relativize $ ngram 4 s | s <- solutions]
  putStrLn $ "Found " ++ show (length solutions) ++ " valid counterpoints:"
  saveCounterpointsToCSV "6.csv" solutions

relativize :: [Int] -> [Int]
relativize xs = zipWith subtract xs (drop 1 xs)

saveCounterpointsToCSV :: FilePath -> [[Int]] -> IO ()
saveCounterpointsToCSV filePath counterpoints = do
  let csvContent = unlines $ map (intercalate "," . map show) counterpoints
  writeFile filePath csvContent

-- * utilities for tidal

{-# NOINLINE gRef #-}
gRef = unsafePerformIO $ newIORef ([] :: [Int])

{-# NOINLINE xRef #-}
xRef = unsafePerformIO $ newIORef ([[]] :: [[Int]])

-- | `xs <- cache (fux1 defaultFux1) cantus`
cache f g = do
  gp <- readIORef gRef
  if g == gp
    then readIORef xRef
    else do
      xn <- f g
      writeIORef xRef xn
      writeIORef gRef g
      return xn
