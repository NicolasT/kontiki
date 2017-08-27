#!/bin/bash -xue

stack -j 2 --no-terminal setup
stack -j 2 --no-terminal build --test --haddock --only-snapshot
stack -j 2 --no-terminal build --haddock grpc-haskell
