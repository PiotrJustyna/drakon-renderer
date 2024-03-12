FROM alpine:3.19.1

WORKDIR "/root/code/drakon-renderer"

RUN \
  apk update \
  && \
  apk add \
    git \
    openssh \
    curl \
    zlib-dev \
    g++ \
    ghc \
    cabal && \
  git config --global --add safe.directory "/root/code/drakon-renderer"

RUN \
  cabal update \
  && \
  cabal install --lib diagrams \
  && \
  cabal install --lib diagrams-lib \
  && \
  cabal install --lib diagrams-svg \
  && \
  cabal install --lib base

CMD [ "/bin/sh" ]
