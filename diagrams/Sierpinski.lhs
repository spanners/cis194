> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude
>
> sierpinski 1 = eqTriangle 1
> sierpinski n =     s
>                   ===
>                (s ||| s) # centerX
>   where s = sierpinski (n-1)
>
> example = pad 1.1 $ sierpinski 7 # centerXY # lw none # fc white
>                   `atop` square 70 # fc darkslategray
