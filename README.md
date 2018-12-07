Unparse attoparsec
==================

This library provides an interface to build programs that can be
interpreted both as parsers and as printers.

This library essentially defines two types:

- `Parser :: * -> * -> *`, a wrapper around `attoparsec`'s own `Parser :: * -> *`;
- `Printer :: * -> * -> *`, a type which does the inverse of a parser;

and a type class `Attoparsec`, instantiated by `Parser` and `Printer`, wrapping
`attoparsec`'s core operations (including lookaheads!).

Monadic and applicative composition can be used under a `ForallF Monad p` constraint
(resp. `ForallF Applicative p`), meaning that for all types `x`, the type `p x`
is an instance of `Monad` (resp. `Applicative`). These constraints are made
possible thanks to the `constraints` package. The
[`profunctor-monad`](https://github.com/Lysxia/profunctor-monad) package
provides convenient ways to work with them (`with` or rebindable syntax).

## Examples

Check out [`example/AesonParser.hs`](https://github.com/Lysxia/unparse-attoparsec/blob/master/example/AesonParser.hs)
