FROM alpine:3.19.1

WORKDIR "/root/code/drakon"

RUN \
  apk update \
  && \
  apk add \
    git \
    openssh \
    curl \
    g++ \
    ghc \
    cabal && \
  git config --global --add safe.directory "/root/code/drakon"

CMD [ "/bin/sh" ]
