FROM haskell:7.8
RUN cabal update && cabal install
