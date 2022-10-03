# overeasy

[![CircleCI](https://circleci.com/gh/ejconlon/overeasy/tree/master.svg?style=svg)](https://circleci.com/gh/ejconlon/overeasy/tree/master)

A purely functional E-Graph library

## Quick start

If you have `stack` installed (see below), try `make ghci` to hop into a REPL. Then try `exampleMain` or anything else from the `Overeasy.Example` module.

## More on how to build and run

This repo is setup so you never have to `cd` out of the root directory.

To run the Haskell programs, you need `stack` installed on your system. [This](https://docs.haskellstack.org/en/stable/README/) is an easy way to do so, but you can also check your package manager or use [ghcup](https://www.haskell.org/ghcup/).

Most of the interesting stuff is going to be run in the test suite. Run it with `make test`. `stack` will get the appropriate Haskell compiler and package dependencies, and it will compile the project before running the test suite.
