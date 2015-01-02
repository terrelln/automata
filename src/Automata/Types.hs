{-# LANGUAGE GADTSyntax, GADTs, Arrows, TypeFamilies, FlexibleContexts #-}

module Automata.Types where

import Control.Arrow
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe

data DFA arrow s a where
    DFA ::  (ArrowChoice arrow, Ord s, Eq s, Ord a, Eq a) =>
            { dfa_Q :: Set s
            , dfa_E :: Set a
            , dfa_D :: arrow (s, a) s
            , dfa_q_0 :: s
            , dfa_F :: Set s
            } -> DFA arrow s a

data NFA arrow s a where
    NFA ::  (ArrowChoice arrow, Ord s, Eq s, Ord a, Eq a) =>
            { nfa_Q :: Set s
            , nfa_E :: Set a
            , nfa_D :: arrow (s, Maybe a) (Set s)
            , nfa_q_0 :: s
            , nfa_F :: Set s
            } -> NFA arrow s a

class FiniteAutomata fa where
    type State fa :: *
    type CurrentState fa :: *
    type Alphabet fa :: *
    type Input fa
    type ArrowType fa :: * -> * -> *

    get_Q :: fa -> Set (State fa)
    get_E :: fa -> Set (Alphabet fa)
    get_D :: fa -> (ArrowType fa) (State fa, Input fa) (CurrentState fa)
    get_q_0 :: fa -> State fa
    get_F :: fa -> Set (State fa)

    toNFA :: (ArrowChoice (ArrowType fa)) => fa -> NFA (ArrowType fa) (State fa) (Alphabet fa)

    toCurrentState :: (ArrowChoice arrow, arrow ~ (ArrowType fa)) => fa -> arrow (State fa) (CurrentState fa)
    currentStateToSet :: (ArrowChoice (ArrowType fa)) => fa -> (ArrowType fa) (CurrentState fa) (Set (State fa))
    
    startFA :: (ArrowChoice arrow, arrow ~ (ArrowType fa)) => fa -> arrow [Alphabet fa] (CurrentState fa, [Alphabet fa])

    transitionFA :: ArrowChoice (ArrowType fa) => fa -> (ArrowType fa) (CurrentState fa, Input fa) (CurrentState fa)

    stepFA :: (ArrowChoice arrow, arrow ~ (ArrowType fa)) => fa -> arrow (CurrentState fa, [Alphabet fa]) (CurrentState fa, [Alphabet fa])

    finishFA :: (ArrowChoice arrow, arrow ~ (ArrowType fa)) => fa -> arrow (CurrentState fa, [Alphabet fa]) (CurrentState fa)
    finishFA automata = proc currState@(q, input) -> case input of
        [] -> arr fst <<< stepFA automata -< currState
        _ -> finishFA automata <<< stepFA automata -< currState

    applyFA :: (ArrowChoice arrow, arrow ~ (ArrowType fa)) => fa -> arrow [Alphabet fa] (CurrentState fa)
    applyFA fa = finishFA fa <<< startFA fa

    acceptFA :: (ArrowChoice arrow, arrow ~ (ArrowType fa)) => fa -> arrow (CurrentState fa) Bool

instance (ArrowChoice arrow, Ord s) => FiniteAutomata (DFA arrow s a) where
    type State (DFA arrow s a) = s
    type CurrentState (DFA arrow s a) = s
    type Alphabet (DFA arrow s a) = a
    type Input (DFA arrow s a) = a
    type ArrowType (DFA arrow s a) = arrow

    get_Q = dfa_Q
    get_E = dfa_E
    get_D = dfa_D
    get_q_0 = dfa_q_0
    get_F = dfa_F

    toNFA (DFA qs as delta q_0 f) = NFA qs as delta' q_0 f                           
        where   delta' = proc (q,sigma) -> case sigma of                             
                        Nothing -> returnA -< S.empty                                
                        Just sigma' -> arr S.singleton <<< delta -< (q, sigma')

    toCurrentState nfa = arr id
    currentStateToSet nfa = arr S.singleton

    startFA dfa = proc input -> returnA -< (dfa_q_0 dfa, input)

    transitionFA = arr get_D

    stepFA dfa = proc (q, input) -> case input of
        [] -> returnA -< (q, input)
        c:cs -> first (transitionFA dfa) -< ((q,c),cs)


    acceptFA dfa = proc q -> returnA -< q `S.member` dfa_F dfa

instance (ArrowChoice arrow, Ord s) => FiniteAutomata (NFA arrow s a) where
    type State (NFA arrow s a) = s
    type CurrentState (NFA arrow s a) = Set s
    type Alphabet (NFA arrow s a) = a
    type Input (NFA arrow s a) = Maybe a
    type ArrowType (NFA arrow s a) = arrow

    get_Q = nfa_Q
    get_E = nfa_E
    get_D = nfa_D
    get_q_0 = nfa_q_0
    get_F = nfa_F

    toNFA = id

    toCurrentState nfa = arr S.singleton
    currentStateToSet nfa = arr id

    startFA nfa = proc input -> returnA -< (S.singleton $ nfa_q_0 nfa, input)
    
    transitionFA nfa = proc (qs, c) -> case c of
        Nothing -> epsilonClosure nfa -< qs
        Just _ -> first (epsilonClosure nfa >>> arr S.toList) >>> runAll nfa -< (qs, c)
        
        where   epsilonClosure nfa = proc qs -> do
                    qs' <- runAll nfa -< (S.toList qs, Nothing)
                    if S.null (qs' S.\\ qs) then returnA -< qs
                    else epsilonClosure nfa -< qs `S.union` qs'
                runAll nfa = proc (qs, c) -> case qs of
                    [] -> returnA -< S.empty
                    q:qs -> do
                        q' <- get_D nfa -< (q, c)
                        qs' <- runAll nfa -< (qs, c)
                        returnA -< q' `S.union` qs'

    stepFA nfa = (second (arr listToMaybe) >>> transitionFA nfa)
                    &&& (arr (drop 1 . snd))
    
    acceptFA nfa = proc qs -> returnA -< not . S.null $ qs `S.intersection` nfa_F nfa
