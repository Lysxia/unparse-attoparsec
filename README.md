Unparse attoparsec
==================

Define an `attoparsec` parser, get a DRY printer.

This library essentially defines two types:

- `Parser :: * -> * -> *`, a wrapper around `attoparsec`'s own `Parser :: * -> *`;
- `Printer :: * -> * -> *`, a type which does the inverse of a parser;

and a type class `Attoparsec`, instantiated by `Parser` and `Printer`, wrapping
`attoparsec`'s core operations (including lookaheads!).

Programming polymorphically under an `Attoparsec p` constraint yields a DRY
specification of both a parser and a printer.

Monadic and applicative composition can be used under a `ForallF Monad p` constraint
(resp. `ForallF Applicative p`), meaning that for all types `x`, the type `p x`
is an instance of `Monad` (resp. `Applicative`). These constraints are made
possible thanks to the `constraints` package. The `profunctor-monad` package 
(not yet available; see `src/Data/Attoparsec/Unparse/Profunctor.hs`)
provides convenient ways to work with them (`with` or rebindable syntax).

## Building

Simply clone the repo' and build with stack:

    stack install --test

## Examples

Check out [`example/AesonParser.hs`](https://github.com/Lysxia/unparse-attoparsec/blob/master/example/AesonParser.hs)
