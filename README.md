# counterpoint for tidalcycles using sbv

## installation
Follow instructions at https://tidalcycles.org/, then add this library

```sh
git clone https://github.com/aavogt/counterpoint.git
cd counterpoint
cabal install --lib
```

## example

```haskell
:set -XRecordWildCards

import Fux

do
    setcps 2
    XS { .. } <- Fux.fux1 Fux.def "c g e g d a4 c"
    d1 $ mconcat [ gs # "supergong",
      xs # "supergong",
      "bd/4 [hc*2|hc/3]" # gain 1.2]
    all (\x -> x # room (range 0 0.2 (slow 2 saw))
     # djf (range 0.25 0.4 (slow nbeat perlin)))
```

```haskell
:set -XRecordWildCards -XDuplicateRecordFields

import Fux
import Data.List

do
  setcps 5
  XS { .. } <- fux1 def{nsol = 20, doh = 55, allowedRange = (40, 72)} "c d e f  g a b g  c e a4 c"
  let inst = degradeBy slowsaw "supervibe" # gain 0.65
       where slowsaw = slow nbeat $ range (slow (2*nbeat) perlin) 0 tri
  let drums = innerJoin $ cycleChoose $ map (slow nbeat . stack) $ tails ["bd(7,12)", "hc(5,12)", "ho", "bass3(5,12,2)" ]
  d1 $ stack [
    gs # inst,
    xs # inst,
    drums
    -- , slow nbeat "bev" # legato 1
    ]
  all (\x -> x # djf 0.3 # krush 4)
```

## TODO

- [ ] fux2 not working: Stream.doTick divide by zero
- [ ] fux2 rhythm output
- [ ] fux1 rhythm input
- [ ] fux3
- [ ] GHC-02256 warning
