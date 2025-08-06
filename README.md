# first species counterpoint for tidalcycles using sbv

## installation
Follow instructions at https://tidalcycles.org/, then add this library

```sh
git clone https://github.com/aavogt/counterpoint.git
cd counterpoint
cabal install --lib
```

## example

```haskell
import Fux1

let nI xs = n (cat (map (pure . fromIntegral . subtract 60) xs))

do
    let asInt str = case parseTPat str of Right (TPat_Seq s) -> map (\(TPat_Atom _ a) -> 60 + a) s
    let gs = asInt "c d e g e e a4 c"
    xs <- cache (fux1 defaultFux1) gs
    d1 $ nI gs # sound "supermandolin" # legato (range 0.5 4 (slow 8 perlin))
    d2 $ randcat [ nI x # sound "supermandolin" | x <- xs ] # legato 0.8 # gain 0.9
    d3 $ sometimes (fast 2) $ "808bd <808hc 808ht>" # gain 1.1
    let p = 6
    let q = 8
    let b q = concatMap ((True:) . flip replicate False . (`mod` p)) $ zipWith subtract gs (xs !! q)
    let c q = map (`div` p) $ zipWith subtract gs (xs !! q)
    d4 $ do
      -- it would be nice to vary p or q slowly.
      -- This way doesn't play arpy, but other join-like functions might work
      -- q <- slow 8 $ _irand (length cps - 1)
      fast 8 $ n (fromList (map fromIntegral (c q))) # struct (fromList (b q)) "arpy" # gain 0.7
```
