import qualified Data.Map as M
import Data.Maybe
import Control.Monad

type MachineState = Int
type TransitionFunction = M.Map (MachineState, Char) MachineState

data State = State  { input :: String
                    , state :: MachineState
                    } deriving (Show, Eq)

data Dfa = Dfa  { states :: [MachineState]
                , alphabet :: String
                , transition :: TransitionFunction
                , startState :: MachineState
                , acceptStates :: [MachineState]
                }

empty :: Dfa
empty = Dfa { states = [], alphabet = [], transition = M.empty, startState = 0, acceptStates = [] }

setTransition :: TransitionFunction -> Dfa -> Dfa
setTransition fn dfa = Dfa { states = states dfa, alphabet = alphabet dfa,
                     transition = fn, startState = startState dfa,
                     acceptStates = acceptStates dfa }

setStates :: [MachineState] -> Dfa -> Dfa
setStates s dfa = Dfa { states = s, alphabet = alphabet dfa,
                     transition = transition dfa, startState = startState dfa,
                     acceptStates = acceptStates dfa }

setAlphabet :: String -> Dfa -> Dfa
setAlphabet a dfa = Dfa { states = states dfa, alphabet = a,
                     transition = transition dfa, startState = startState dfa,
                     acceptStates = acceptStates dfa }

setStartState :: MachineState -> Dfa -> Dfa
setStartState q_0 dfa = Dfa { states = states dfa, alphabet = alphabet dfa,
                     transition = transition dfa, startState = q_0,
                     acceptStates = acceptStates dfa }

setAcceptStates :: [MachineState] -> Dfa -> Dfa
setAcceptStates f dfa = Dfa { states = states dfa, alphabet = alphabet dfa,
                     transition = transition dfa, startState = startState dfa,
                     acceptStates = f }

step :: Dfa -> State -> Maybe State
step dfa s
    | null $ input s    = Just s
    | isNothing q'      = Nothing
    | otherwise         = Just s'
        where   s' = State { input = as, state = d M.! (q,a) }
                q  = state s
                (a:as) = input s
                d = transition dfa
                q' = M.lookup (q,a) d

run :: Dfa -> State -> [Maybe State]
run dfa s = iterate (>>= step dfa) (Just s)

start :: Dfa -> String -> State
start dfa w = s
    where   s = State { input = w, state = q_0 }
            q_0 = startState dfa

simulate :: Dfa -> String -> Maybe State
simulate dfa w = run dfa (start dfa w) !! length w

accept :: Dfa -> Maybe State -> Bool
accept _ Nothing = False
accept dfa (Just s) = q `elem` f
    where   q = state s
            f = acceptStates dfa

extendTransition :: TransitionFunction -> TransitionFunction -> TransitionFunction
extendTransition = M.union

toTransition :: [((MachineState, Char), MachineState)] -> TransitionFunction
toTransition = M.fromList

myDfa :: Dfa
myDfa = Dfa { states = qs, alphabet = a, startState = q_0, acceptStates = f,
        transition = toTransition d }
    where   qs = [1..5]
            q_0 = 1
            f = [4,5]
            a = "01"
            d = [((1,'0'),2), ((1,'1'),4), ((2,'0'),2), ((2,'1'),3), ((3,'0'),2), ((3,'1'),4), ((4,'0'),5), ((4,'1'),4), ((5,'0'),5), ((5,'1'),4)]



main = forever $ do
        w <- getLine
        let q = simulate myDfa w
        print $ accept myDfa q
