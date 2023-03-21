
module Par ((|||)) where

import GHC.Conc

infix 1 |||

(|||) :: a -> b -> (a,b)
a ||| b = a `par` b `par` (a,b)
