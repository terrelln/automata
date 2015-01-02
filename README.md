# automata

The goal of this package is to enable simple and fast simulation and visualization of finite automata.

Currently I have a (maybe) working implementation of DFA and NFA simulation.
Please excues the messy code, this project is still under *heavy* development
and changing rapidly.

##  DFA & NFA Simulation

My goals for this package are:

*   Make creating simple DFAs and NFAs easy enough (not there yet)
*   Check user input DFAs and NFAs for validity (haven't started this one)
*   Make it easy to combine and modify DFAs and NFAs, examples being:

    *   Intersection (done)
    *   Reversal (done)
    *   Union (easy to do)
    *   Concatenation (just as easy to do)
    *   Other transformations (I have to think of them first)

*   Make the interface general enough to be useful.

I think I am a good way towards my goal, although I may have over-engineered
the types a bit.

One of the best design decisions I made‒in my opinion—is to represent the
transition function as arbitrary arrows.

```haskell
dfaTransition :: ArrowChoice ar => ar (state, alphabet) -> state
nfaTransition :: ArrowChoice ar => ar (state, Maybe alphabet) -> Set state
```

The NFA transition takes a `Maybe alphabet` to handle epsilon-transitions.
This representation allows me to embed debugging/error-handling code into the
transition function.
For instance, I can use a Kleisli arrow to embed an `Either` or `Maybe` monad
and have my transition function that returns an error message for unreachable
states:

```haskell
data S = Q0 | Q1 | QReserved

delta Q0 _ = Right Q1
delta Q1 _ = Right Q0
delta q  c = Left (q,c)

transitionFunction = Kleisli (uncurry delta)
```

I could also use it to log information in the same way by embedding the `IO`
monad.  An example of this is in `examples/fizzbuzz.hs`, where I use the
intersection of two DFAs to pass the FizzBuzz challenge.
