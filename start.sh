#!/bin/bash

CERTIFICATE_DIRECTORY="$HOME/.ssh/"

docker build \
  -t "drakon-renderer:latest" \
  -f "dockerfile" \
  . \
&& \
docker run \
  -it \
  -v "$(pwd):/tmp/code" \
  -v "$CERTIFICATE_DIRECTORY:/home/$USER_NAME/.ssh:ro" \
  -v "$CERTIFICATE_DIRECTORY:/root/.ssh:ro" \
  --rm "drakon-renderer:latest"
  