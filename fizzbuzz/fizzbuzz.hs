module Main where

import Automata.Types
import Automata.Combinators
import Control.Arrow
import Control.Monad

data S = Q0 | Q1 | Q2 | Q3 | Q4 deriving (Show, Eq, Ord)

c = const

fizz = makeDFA [1..100] Q0 [] [
        (Q0, c Q1),
        (Q1, c Q2),
        (Q2, c Q0)
    ] :: DFA (->) S Int

printFizz = DFA (get_Q fizz) (get_E fizz) (Kleisli delta') (get_q_0 fizz) (get_F fizz)
    where   delta' (q,a) = do
                putStr (show a ++ ": ")
                when (q == Q2) (putStr "Fizz")
                return $ get_D fizz (q,a)

buzz = makeDFA [1..100] Q0 [] [
        (Q0, c Q1),
        (Q1, c Q2),
        (Q2, c Q3),
        (Q3, c Q4),
        (Q4, c Q0)
    ] :: DFA (->) S Int

printBuzz = DFA (get_Q buzz) (get_E buzz) (Kleisli delta') (get_q_0 buzz) (get_F fizz)
    where   delta' (q,a) = do
                when (q == Q4) (putStr "Buzz")
                putStr "\n"
                return $ get_D buzz (q,a)

fizzBuzz = intersectFA printFizz printBuzz

main = (runKleisli $ applyFA fizzBuzz) [1..100]
