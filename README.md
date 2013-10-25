random-generator 0.1

random-generator is trying to answer the following question:

> What is an elegant interface for random value generation?

The focus is to get a combinator library where small, composable
combinators are composed to express rich behaviors. I tried hard to
choose type definitions that avoid duplication of concerns between the
various aspects of the library, building domain-specific notions of
random generation on top of a simple base layer in a modular way.

(Note that this focus is sometimes in tension with providing
convenient, derived functions that make user's code short and easy to
read. For now, random-generator will choose the "nice for the library
designer" way, and code may require some time learning the library to
be easy to read. You have been warned.)

The library currently provides three different pieces:

- a type `'a gen` for dead simple random generation
- a type `'a backtrack_gen` for generators that can fail:
  "generate a value such that this (possibly empty) condition is verified"
- a type `'a fueled` for generation of values with an inductive
  (tree-like) structure that looks nice to the human eye; see [the
  documentation](http://gasche.github.io/random-generator/doc/Generator.html#2_fueledgenerators) for more information on this.

I consider the value of this library to be in its interface, not
necessarily its implementation. I think the current interface is solid
(though it can still be improved) and encourage people writing random
generators to steal and reuse it -- or at least feel inspired by it.

## Versioning

I consider that I can (and will) break backward compatibility by
changing the interface for any 0.x release.
