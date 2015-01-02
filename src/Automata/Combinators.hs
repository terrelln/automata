{-# LANGUAGE Arrows, GADTs, FlexibleContexts #-}
module Automata.Combinators where

import Automata.Types
import Automata.Helpers
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Arrow
import Data.Maybe
import Data.List (foldl')

makeDFA :: (ArrowChoice arrow, Ord s, Ord a) => [a] -> s -> [s] -> [(s, a -> s)] -> DFA arrow s a
makeDFA alphabet q_0 accepts transitionList = DFA states (S.fromList alphabet) (arr delta) q_0 (S.fromList accepts)
    where   map = M.fromList transitionList
            states = M.keysSet map
            delta (s, a) = (map M.! s) a

makeNFA :: (ArrowChoice arrow, Ord s, Ord a) => [a] -> s -> [s] -> [(s,Maybe a -> [s])] -> NFA arrow s a
makeNFA alpha q_0 accepts transitionList = NFA states (S.fromList alpha) (arr delta) q_0 (S.fromList accepts)
    where   ts = map (second (>>> S.fromList)) transitionList
            m = M.fromList ts
            states = M.keysSet m
            delta (s,a) = (m M.! s) a

reverseTransitionFA ::  (FiniteAutomata fa, ArrowChoice (ArrowType fa),
                        Ord (State fa), Ord (Alphabet fa)) =>
                        fa -> NFA (ArrowType fa) (State fa) (Alphabet fa)
reverseTransitionFA auto = NFA states (get_E auto) delta' (get_q_0 auto) (get_F auto)
        where   states = get_Q auto
                delta' = proc (q, sigma) ->
                    reverseOne -< (S.toList states, q, sigma)
                reverseOne = proc (cQ, q, sigma) -> case cQ of
                    [] -> returnA -< S.empty
                    p:ps -> do
                        (p',_) <- first (currentStateToSet auto) <<< stepFA auto <<< first (toCurrentState auto) -< (p, maybeToList sigma)
                        ps' <- reverseOne -< (ps, q, sigma)
                        returnA -< if q `S.member` p' then p `S.insert` ps' else ps'

switchStartAcceptNFA :: (ArrowChoice arrow, Ord s, Ord a) =>
                        s -> NFA arrow s a -> NFA arrow s a
switchStartAcceptNFA q_0 nfa = NFA states (get_E nfa) delta q_0 accepts
    where   states = q_0 `S.insert` get_Q nfa
            accepts = S.singleton $ get_q_0 nfa
            delta = proc (q, sigma) -> if q == q_0
                then returnA -< if isNothing sigma then get_F nfa else S.empty
                else arr fst <<< stepFA nfa -< (S.singleton q, maybeToList sigma)

reverseFA ::    (FiniteAutomata fa, ArrowChoice (ArrowType fa),
                Ord (State fa), Ord (Alphabet fa)) =>
                State fa -> fa -> NFA (ArrowType fa) (State fa) (Alphabet fa)
reverseFA q_0 = reverseTransitionFA >>> switchStartAcceptNFA q_0

intersectFA ::  (FiniteAutomata fa1, FiniteAutomata fa2,
                ArrowChoice (ArrowType fa1), ArrowType fa1 ~ ArrowType fa2,
                Ord (Alphabet fa1), Alphabet fa1 ~ Alphabet fa2,
                Ord (State fa1), Ord (State fa2)) =>
                fa1 -> fa2 -> NFA (ArrowType fa1) (State fa1, State fa2) (Alphabet fa1)
intersectFA auto1 auto2 = NFA newQ newE delta' q_0' newF
    where   newQ = get_Q auto1 `cartProd` get_Q auto2
            newE = get_E auto1 `S.union` get_E auto2
            q_0' = (get_q_0 auto1, get_q_0 auto2)
            newF = get_F auto1 `cartProd` get_F auto2
            delta' = ((first (arr fst >>> arr S.singleton) >>> transitionFA (toNFA auto1)) &&&
                     (first (arr snd >>> arr S.singleton) >>> transitionFA (toNFA auto2))) >>>
                     arr (uncurry cartProd)
