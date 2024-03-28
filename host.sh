#!/bin/sh

CERTIFICATE_DIRECTORY="$HOME/.ssh/"

docker buildx build \
  -t "drakon-renderer:latest" \
  -f "dockerfile" \
  . \
&& \
docker run \
  -it \
  -v "$(pwd):/root/code/drakon-renderer" \
  -v "$CERTIFICATE_DIRECTORY:/root/.ssh:ro" \
  "drakon-renderer:latest"
