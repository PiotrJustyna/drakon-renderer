FROM alpine:3.20.1

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
  cabal install hlint \
  && \
  echo "PROMPT='%F{green}%*UTC%f %F{cyan}%n%f %F{magenta}%~%f $ '" >> ~/.zshrc

# 2024-06-03 PJ:
# --------------
# this is where hlint is installed
ENV PATH="${PATH}:/root/.local/bin"

CMD [ "/bin/zsh" ]