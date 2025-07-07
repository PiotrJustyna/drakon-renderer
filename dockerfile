FROM alpine:3.21.3

WORKDIR "/root/code/drakon-renderer"

RUN apk update
RUN apk add \
    bash \
    git \
    openssh \
    curl \
    zlib-dev \
    g++ \
    ghc \
    cabal \
    ncurses-dev
RUN git config --global --add safe.directory "/root/code/drakon-renderer"
RUN cabal update
RUN cabal install hindent
RUN cabal install hlint
RUN cabal install alex
RUN cabal install happy

# 2024-12-02 PJ:
# --------------
# this is where executables
# installed with cabal install are located
ENV PATH="${PATH}:/root/.local/bin"

CMD [ "/bin/bash" ]
