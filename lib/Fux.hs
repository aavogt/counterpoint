module Fux (module Fux, module Data.Default) where

import Control.Lens hiding ((.>))
import Control.Lens.Action
import Control.Monad
import Data.Default
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.SBV
import Data.SBV.Dynamic hiding (allSatWith)
import Data.SBV.Internals
import Debug.Trace
import Sound.Tidal.Boot (cat, n)
import Sound.Tidal.ParseBP
import Sound.Tidal.Pattern
import System.IO.Unsafe (unsafePerformIO)
import Text.Read

-- | Configuration for first species counterpoint
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

-- | generate all valid 1st species counterpoints
fux1 :: Fux1 -> [Int] -> IO [[Int]]
fux1 config@Fux1 {..} cantus = do
  if null (drop 2 cantus)
    then return []
    else do
      solutions <- allSatWith defaultSMTCfg {allSatMaxModelCount = Just nsol} $ do
        counterpoint <- sIntegers $ map (("cp" ++) . show) (zipWith const [0 ..] cantus)
        applyFuxConstraints config cantus counterpoint
      print solutions
      return $ extractSolutions solutions

-- Apply all first species counterpoint constraints
applyFuxConstraints :: Fux1 -> [Int] -> [SInteger] -> Symbolic ()
applyFuxConstraints config@Fux1 {..} (map fromIntegral -> gs) xs = do
  let lag x = zip x (drop 1 x)

  -- absolute difference between xs and gs
  -- xg interval
  let adxgs = [abs (x - g) `sEMod` 12 | (x, g) <- zip xs gs]

  constrain $
    sum [oneIf @Integer (x .== y) | (x, y) <- lag xs]
      .<= fromIntegral retriggers

  -- Range constraints
  do
    let (minNote, maxNote) = allowedRange & each %~ fromIntegral
    constrain $ sAll (.>= minNote) xs
    constrain $ sAll (.<= maxNote) xs

  -- each simultaneous pair cannot be dissonant
  constrain $ sAll (`sNotElem` [1, 2, 6, 10, 11]) adxgs

  -- Voice crossing constraints
  case voicePosition of
    _ | allowCrossing -> return ()
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
    (g : _, _ : xP : _) -> constrain $ applyCadence cadenceType g xP
    _ -> return ()

data Fux2 = Fux2
  { cadenceType :: CadenceType,
    voicePosition :: VoicePosition, -- counterpoint above or below cantus firmus
    allowCrossing :: Bool, -- allow voices to cross
    allowedRange :: (Int, Int), -- MIDI range for counterpoint voice

    -- | number of repeated notes in the voice
    retriggers :: Int,
    nsol :: Int
  }

-- | generate all 2nd species counterpoints with the given rhythm
fux2 ::
  Fux2 ->
  -- | `[(cantusNote, twoCounterpointNotes?)]`
  [(Int, Bool)] ->
  IO [[Int]]
fux2 config@Fux2 {..} (unzip -> (cantus, duple)) = do
  -- expand cantus to match counterpoint positions (1 or 2 notes per cantus)
  let gs = concat [replicate (if d then 2 else 1) g | (g, d) <- zip cantus duple]
  if length cantus < 3
    then return []
    else do
      solutions <- allSatWith defaultSMTCfg {allSatMaxModelCount = Just nsol} $ do
        counterpoint <- sIntegers $ map (("cp" ++) . show) [0 .. length gs - 1]
        applyFux2Constraints config cantus duple counterpoint
      print solutions
      return $ extractSolutions solutions

applyFux2Constraints :: Fux2 -> [Int] -> [Bool] -> [SInteger] -> Symbolic ()
applyFux2Constraints config@Fux2 {..} cantus duple xs = do
  let lag x = zip x (drop 1 x)
      rep d = if d then 2 else 1
      sizes = map rep duple
      starts = init (scanl (+) 0 sizes) -- start index for each cantus note
      gsBase = map fromIntegral cantus
      gs = concat [replicate (rep d) g | (g, d) <- zip gsBase duple]
      strongMask = concat [if d then [True, False] else [True] | d <- duple]
      adxgs = [abs (x - g) `sEMod` 12 | (x, g) <- zip xs gs]
      adxgsStrong = [a | (a, s) <- zip adxgs strongMask, s]
      isConsonant ivl = ivl `sNotElem` [1, 2, 6, 10, 11]

  -- retriggers (consecutive equal notes)
  constrain $
    sum [oneIf @Integer (x .== y) | (x, y) <- lag xs]
      .<= fromIntegral retriggers

  -- range
  do
    let (minNote, maxNote) = allowedRange & each %~ fromIntegral
    constrain $ sAll (.>= minNote) xs
    constrain $ sAll (.<= maxNote) xs

  -- voice crossing
  case voicePosition of
    _ | allowCrossing -> return ()
    Above -> constrain $ sAnd $ zipWith (.>=) xs gs
    Below -> constrain $ sAnd $ zipWith (.<=) xs gs

  -- no big jumps
  constrain $ sAll (\(a, b) -> abs (a - b) .<= 12) (lag xs)

  -- strong beats must be consonant
  constrain $ sAll isConsonant [a | (a, s) <- zip adxgs strongMask, s]

  -- weak beats: consonant OR (passing tone: step-step in same direction)
  forM_ (zip3 [0 ..] starts sizes) $ \(j, s, sz) ->
    when (sz == 2) $ do
      let w = s + 1
      if j + 1 < length starts
        then do
          let p = s
              n = starts !! (j + 1)
              dw1 = xs !! w - xs !! p
              dw2 = xs !! n - xs !! w
              passing = abs dw1 .== 1 .&& abs dw2 .== 1 .&& (dw1 * dw2 .> 0)
          constrain $ isConsonant (adxgs !! w) .|| passing
        else
          -- last weak position: require consonance (no next strong to pass to)
          constrain $ isConsonant (adxgs !! w)

  -- no parallel 5ths/octaves across successive strong beats
  constrain $
    sAnd
      [ i ./= k .|| j ./= k
        | k <- [0, 7],
          (i, j) <- lag adxgsStrong
      ]

  -- no 3 same imperfect consonances in a row on strong beats
  constrain $
    sAnd
      [ sAny (./= m) ijk
        | m <- [3, 4, 8, 9],
          ijk <- map (take 3) $ takeWhile (not . null . drop 2) $ tails adxgsStrong
      ]

  -- start: perfect 5th or 8ve
  adxgs ^!? _head . act \x -> constrain (x `sElem` [0, 7])

  -- end: unison
  adxgs ^!? _last . act \x -> constrain (x .== 0)

  -- cadence relative to last cantus note, using penultimate cp note
  case (reverse gs, reverse xs) of
    (g : _, _ : xP : _) -> constrain $ applyCadence cadenceType g xP
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

instance Default Fux1 where
  def =
    Fux1
      { cadenceType = Plagal,
        voicePosition = Above,
        allowCrossing = False,
        retriggers = 2,
        allowedRange = (60, 72), -- Middle C to C5
        nsol = 5000
      }

instance Default Fux2 where
  def =
    Fux2
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

exampleRhythm = cycle [False, True, False]

ngram n = takeWhile ((== n) . length) . map (take n) . tails

runExample :: IO ()
runExample = do
  putStrLn "Generating counterpoints for cantus firmus: C-D-E-D-C"
  solutions <- fux2 def (exampleCantus `zip` exampleRhythm)

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

-- | `xs <- cache (fux1 def) cantus`
cache f g = do
  gp <- readIORef gRef
  if g == gp
    then readIORef xRef
    else do
      xn <- f g
      writeIORef xRef xn
      writeIORef gRef g
      return xn

-- | return type for `transposer`
data TP = TP {doh :: Int, nI :: [Int] -> ControlPattern, asInt :: String -> [Int]}

transposer doh =
  let nI xs = n $ cat $ pure . fromIntegral . subtract doh <$> xs
      asInt str = case parseTPat str of Right (TPat_Seq s) -> map (\(TPat_Atom _ a) -> doh + a) s
   in TP {..}
