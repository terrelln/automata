{-# LANGUAGE TemplateHaskell, Arrows, GADTs, TypeFamilies #-}

module Main where

import System.Exit
import Test.QuickCheck
import Control.Monad
import qualified Data.Set as S
import Data.Maybe
import Control.Arrow

import Automata.Types
import Automata.Combinators
import Automata.Helpers

data MyState = QStart | Q0 | Q1 | QDead deriving(Ord, Eq, Enum, Bounded, Show)   
data Character = Zero | One deriving(Ord, Eq, Enum, Bounded, Show)

f :: MyState -> MyState -> Character -> MyState
f q _ Zero = q
f _ q One = q

dfa = makeDFA [Zero,One] Q0 [Q0,Q1] [
        (Q0, f Q0 Q1),
        (Q1, f QDead Q1),
        (QDead, const QDead)
    ] :: DFA (->) MyState Character
                                                                                 
nfaR = reverseFA QStart dfa                                                      
nfaRI = intersectFA dfa nfaR                                                     
                                                                                 
parse :: Bool -> Character                                                       
parse False = Zero                                                               
parse True = One                                                                 
                                                                                 
runFA fa input = (acceptFA fa . applyFA fa) (fmap parse input)                   
                                                                                 
isSorted :: [Bool] -> Bool                                                       
isSorted [] = True                                                               
isSorted [i] = True                                                              
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

prop_isSortedDFA input = f1 == p
    where   p = isSorted input
            f1 = runFA dfa input

prop_isSortedNFA input = f1 == p
    where   p = isSorted input
            f1 = runFA (toNFA dfa) input

prop_reverse_correct input = runFA dfa input == runFA nfaR (reverse input)

prop_intersect_correct input = runFA nfaRI input == (f1 && f2)
    where   f1 = runFA dfa input
            f2 = runFA nfaR input

lNegHalf' :: (ArrowChoice arrow, Ord s, Ord a) => s -> DFA arrow s a -> NFA arrow (s,s) a
lNegHalf' q_0 = ((reverseTransitionFA &&& id) >>> uncurry intersectFA) &&& id >>> uncurry finishMod
    where   finishMod nfa origDfa = NFA states (get_E nfa) delta start accepts   
                where   states = start `S.insert` get_Q nfa                      
                        start = (q_0, q_0)                                       
                        accepts = S.singleton (get_q_0 origDfa) `cartProd` get_F origDfa
                        qq = S.fromList [(q,q) | q <- S.toList (get_Q origDfa)]  
                        delta = proc (q, sigma) -> if start == q                 
                            then returnA -< if isNothing sigma then qq else S.empty
                            else transitionFA nfa -< (S.singleton q, sigma)

nfa12 = lNegHalf' QStart dfa

allEq [] = True                                                                  
allEq [a] = True                                                                 
allEq (x:y:ys) = (x == y) && allEq (y:ys)

prop_complex_correct input = runFA nfa12 input == runFA dfa (reverse input ++ input)

return []
runAll = $quickCheckAll

main = do
--    quickCheck prop_complex_correct
--    quickCheck prop_intersect_correct
--    quickCheck prop_reverse_correct
--    quickCheck prop_isSorted
    success <- runAll
    unless success exitFailure
