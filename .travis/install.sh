#!/bin/bash -xue

travis_wait stack -j 2 --no-terminal setup
travis_wait stack -j 2 --no-terminal build --test --haddock --only-snapshot
travis_wait stack -j 2 --no-terminal build --haddock grpc-haskell
