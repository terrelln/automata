module Automata.Helpers where

import Data.Set (Set)
import qualified Data.Set as S
import Automata.Types

cartProd :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
cartProd set1 set2 = S.fromList [(x,y) | x <- S.toList set1, y <- S.toList set2]
