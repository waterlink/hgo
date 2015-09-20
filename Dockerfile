FROM haskell:7.8

RUN cabal update

ADD ./hgo.cabal.4.7 /opt/app/hgo.cabal

WORKDIR /opt/app

RUN cabal install --only-dependencies -j4
