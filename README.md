# Staged updates

This repo contains files for my Nov 6, 2023 [NYPLW talk](https://nylanguageworkshop.tumblr.com/post/732755229624467456/workshop-monday-november-6th-simon-charlow)

Here are some direct links:

- [Download the slides](https://raw.githubusercontent.com/schar/nyplw/main/nyplw_2023.pdf)
- [Download the slides (4up)](https://raw.githubusercontent.com/schar/nyplw/main/nyplw_2023_ho.pdf)
- [Haskell code](https://github.com/schar/nyplw/tree/main/code)

## Abstract

Dynamic semantics treats sentence meanings as instructions for updating a body
of information, and this perspective delivers beautiful, insightful, and
appealingly general theories of anaphora. However, these theories don't
validate double-negation elimination (DNE): dynamic negation is anaphorically
opaque, and two negations are no less opaque than one. This prevents us from
extending the insights of dynamic semantics to sentences with two negations
(which license anaphora as well as positive sentences do) and Partee
disjunctions ("either there isn't a bathroom here, or it's very well hidden").
While there have been several attempts to extend or amend dynamic semantics
with a more classical negation, consensus on the right approach is elusive,
and how these accounts relate to each other isn't well understood.

I'll propose an abstract, minimal interface for characterizing dynamic systems
that validate DNE, and show how you can upgrade your favorite dynamic theory
of anaphora into one that validates DNE. I'll discuss several different but
equivalent concrete implementations of this abstract interface, show how to
travel between them, and argue that existing theories of dynamic DNE, though
superficially different, can be seen as manifestations of the same abstract
core. In a nutshell, these theories all provide the means to stage dynamic
updates, and execute staged updates on demand. We'll use simple algebraic
manipulations to construct simple isomorphisms between "competing" theories
and translate insights from one dynamic setting to another. If time permits,
I'll discuss a fascinating connection between DNE and the exceptional scope
properties of indefinites.

