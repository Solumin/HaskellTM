## HaskellTM: Haskell Turing Machine Emulator

I'm currently enrolled in CS 601 Computational Complexity. Though we touch on
Turing machines only briefly, I still find it useful to visualize them.
Therefore I've written this (very basic!) TM emulator.

Current state: All one file, TM support logic tied in with machine-specific
logic.

Next steps: Move TM logic into separate "library". Create proper TM record?

A Turing machine is typically defined as the tuple (Q, Ʃ, δ, S):

- Q = set of all states
- Ʃ = the language of the VM, namely a start symbol, a blank, 0 and 1
- δ = the transformation function
- S = the starting state

In this emulator, Q is implicit in the δ function, Σ is fixed as above, and S is
assumed to be "S". Additionally, δ is a function of the type "DeltaFunction"
that takes a (State, Symbol) and returns (TapeAction, HeadAction, State).

The current δ functions:

- shiftRight (input: (Zero|One)+)
- shiftLeft (input: Blank\*(Zero|One)+)
- binAdd (input: (Zero|One){m}Blank(Zero|One){m}, m > 0)
