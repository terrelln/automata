module Multitape
    ( State
    , Automata
    ) where

import Control.Monad

data State a b = State  { state :: a
                        , tape :: b
                        }

class Automata a where
    step :: (Eq b) => a -> State b c -> Maybe (State b c)
    run :: (Eq b) => a -> State b c -> Maybe [State b c]
    run a s = sequence $ iterate (>>= step a) (Just s)
