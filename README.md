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
-- juno-example.mp3
import Fux1
do
    setcps 0.5
    let inst = mconcat [sound "juno" # djf (range 0.2 0.8 (slow 2 perlin)) # lpq (range 0.4 0.5 perlin),
            off 0.125 (|+ slow 2 (choose [2,7])) "arpy"]
    let doh = 60
    let nI xs = n $ cat $ pure . fromIntegral . subtract doh <$> xs
    let asInt str = case parseTPat str of Right (TPat_Seq s) -> map (\(TPat_Atom _ a) -> doh + a) s
    let gs = asInt "c d e g e e a4 c"
    xs <- cache (fux1 defaultFux1) gs
    let f q p = mconcat [fast 8 $ n c # struct b inst # gain 0.9,
              nI (xs !! q) # inst]
            where
            b = fromList $ concatMap ((True:) . flip replicate False . (`mod` p)) $ map (subtract (head gs)) (xs !! q)
            c = fromList $ map fromIntegral $ map (`div` p) $ zipWith subtract gs (xs !! q)
    let mq = slow 4 (_irand (length xs - 1))
    let mp = slow 4 (1 |+ _irand 3)
    d1 $ mconcat [nI gs # inst, innerJoin $ f <$> mq <*> mp, "{bd*4, hc ~ }" # gain 0.8 ]
```
