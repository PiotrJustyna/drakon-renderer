#!/bin/sh

# 2024-12-04 PJ:
# --------------
# At the time of writing this, these directories are covered here:
# https://cabal.readthedocs.io/en/latest/config.html#directories
# Mounting them as volumes allows us to reuse cabal files
# between containers saving a significant amount of build time.
docker volume create cabal-config
docker volume create cabal-cache
docker volume create cabal-state

docker buildx build \
  -t "drakon-renderer:latest" \
  -f "dockerfile" \
  . \
&& \
docker run \
  -it \
  -v "$HOME/.ssh/:/root/.ssh:ro" \
  -v "$(pwd):/root/code/drakon-renderer" \
  -v cabal-config:/root/.config/cabal \
  -v cabal-cache:/root/.cache/cabal \
  -v cabal-state:/root/.local/state/cabal \
  --rm "drakon-renderer:latest"
