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
      xs_ (slow 8 (_irand nmax)) # "supergong",
      "bd/4 [hc*2|hc/3]" # gain 1.2]
    all (\x -> x # room (range 0 0.2 (slow 2 saw))
     # djf (range 0.25 0.4 (slow 8 perlin)))
```
