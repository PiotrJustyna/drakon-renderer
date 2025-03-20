FROM alpine:3.21.3

WORKDIR "/root/code/drakon-renderer"

RUN \
  apk update \
  && \
  apk add \
    zsh \
    git \
    openssh \
    curl \
    zlib-dev \
    g++ \
    ghc \
    cabal \
    ncurses-dev \
  && \
  git config --global --add safe.directory "/root/code/drakon-renderer" \
  && \
  cabal update \
  && \
  cabal install hindent \
  && \
  echo "PROMPT='%F{cyan}%n%f %F{magenta}%~%f $ '" >> ~/.zshrc

# 2024-12-02 PJ:
# --------------
# this is where executables
# installed with cabal install are located
ENV PATH="${PATH}:/root/.local/bin"

CMD [ "/bin/zsh" ]
