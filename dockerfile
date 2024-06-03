FROM alpine:3.20.0

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
    cabal \
    zsh \
    ncurses-dev \
  && \
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" \
  && \
  git config --global --add safe.directory "/root/code/drakon-renderer" \
  && \
  cabal update \
  && \
  cabal install hlint

# 2024-06-03 PJ:
# --------------
# this is where hlint is installed
ENV PATH="${PATH}:/root/.local/bin"

CMD [ "/bin/zsh" ]