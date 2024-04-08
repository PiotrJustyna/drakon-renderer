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
    cabal \
    zsh \
  && \
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" \
  && \
  git config --global --add safe.directory "/root/code/drakon-renderer" \
  && \
  cabal update

CMD [ "/bin/zsh" ]