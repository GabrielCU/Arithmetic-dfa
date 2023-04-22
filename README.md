# Implementation of a Deterministic Finite Automaton (DFA)

This code defines a Deterministic Finite Automaton (DFA) to identify valid arithmetic expressions.

Author: Gabriel Cordova   
Date: 2023-04-21

## Main function: arithmetic-lexer

```racket
(define (arithmetic-lexer strng))
```

This function receives a string and returns whether the string belongs to the language defined by the DFA. It uses the `evaluate-dfa` function and the defined `delta-arithmetic` transition function.

## Helper function: evaluate-dfa

```racket
(define (evaluate-dfa dfa-to-evaluate strng))
```

This function verifies if a string is acceptable by a DFA. It takes a DFA definition and a string as input and returns either a list of tokens with their types, in the order they appear, or the symbol `invalid`.

## Helper function: char-operator?

```racket
(define (char-operator? char))
```

This function checks if a given character represents an arithmetic operator. It returns true if the character is an operator, and false otherwise.

## Transition function: delta-arithmetic

```racket
(define (delta-arithmetic state char))
```

This function defines the transition rules for the DFA. It takes the current state and a character as input and returns a new state and a token type if a token is found.

The initial state is `start`, and the accept states are `int`, `float`, `exp`, `var`, and `spa`.

### State descriptions

- `start`: The starting state.
- `sign`: The current character is a sign (+ or -).
- `int`: The current character is an integer.
- `dot`: The current character is a dot (.).
- `float`: The current character is a float.
- `e`: The current character is an exponent (e or E).
- `e_sign`: The current character is a sign in the exponent part.
- `exp`: The current character is an exponentiated number.
- `var`: The current character is a variable.
- `op`: The current character is an operator.
- `spa`: The current character is a space.
- `op_spa`: The current character is a space after an operator.
- `inv`: The current character is invalid.

The transition function handles different scenarios for each state, updating the state according to the DFA rules.
