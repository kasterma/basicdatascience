FROM alpine:3.6

LABEL maintainer="kasterma@kasterma.net"
LABEL description="Base image for playing with kafka"
ARG VERSION
LABEL version=$VERSION

RUN apk update && \
  apk add --no-cache bash=4.3.48-r1 openjdk8-jre=8.131.11-r0 && \
  rm -rf /var/cache/apk/*

RUN echo "@testing http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
ADD apk add confd@testing