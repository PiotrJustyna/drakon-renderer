FROM alpine:3.19.0

ARG USER_NAME=root

RUN \
  apk update \
  && \
  apk add \
    git \
    openssh \
    dotnet7-sdk \
    curl

WORKDIR "/tmp/code"

RUN git config --global --add safe.directory "/tmp/code"

CMD [ "/bin/sh" ]
