module Main where

import Automata.Types
import Automata.Combinators
import Control.Arrow
import Control.Monad

-- The state type for the DFAs
data State5 = Q0 | Q1 | Q2 | Q3 | Q4 deriving (Show, Eq, Ord)

-- A DFA using the (->) arrow, with State5 states, over the alphabet {1,...,100}
fizz = makeDFA [1..100] Q0 [] [
        (Q0, const Q1),
        (Q1, const Q2),
        (Q2, const Q0)
    ] :: DFA (->) State5 Int

-- Embedding an IO monad into the DFA to print out fizz.
-- I guess it isn't really a DFA anymore but thats okay.
printFizz :: DFA (Kleisli IO) State5 Int
printFizz = DFA (get_Q fizz) (get_E fizz) (Kleisli delta') (get_q_0 fizz) (get_F fizz)
    where   delta' (q,a) = do
                putStr (show a ++ ": ")
                when (q == Q2) (putStr "Fizz")
                return $ get_D fizz (q,a)

buzz = makeDFA [1..100] Q0 [] [
        (Q0, const Q1),
        (Q1, const Q2),
        (Q2, const Q3),
        (Q3, const Q4),
        (Q4, const Q0)
    ] :: DFA (->) State5 Int

printBuzz :: DFA (Kleisli IO) State5 Int
printBuzz = DFA (get_Q buzz) (get_E buzz) (Kleisli delta') (get_q_0 buzz) (get_F fizz)
    where   delta' (q,a) = do
                when (q == Q4) (putStr "Buzz")
                putStr "\n"
                return $ get_D buzz (q,a)

-- Take the intersection of the two "DFAs" that we constructed.
-- The intersection is defined as:
--     D = (Q, E, delta, q_0, F)
--     D' = (Q', E', delta', q_0', F')
--     intersectFA D D' = (Q x Q', E union E', delta'', (q_0,q_0'), F x F')
--     delta'' ((p1,p2),s) = (delta(p1,s), delta'(p2,s))
fizzBuzz = intersectFA printFizz printBuzz

main = (runKleisli $ applyFA fizzBuzz) [1..100]
