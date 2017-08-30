#!/bin/bash -xue

stack --no-terminal docker pull
stack -j 2 --no-terminal --docker setup
stack -j 2 --no-terminal --docker build --test --haddock --only-snapshot
stack -j 2 --no-terminal --docker build --haddock grpc-haskell
