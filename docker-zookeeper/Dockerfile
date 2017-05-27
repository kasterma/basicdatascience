FROM kasterma/base-ke:0.1.2

ARG VERSION

LABEL maintainer="kasterma@kasterma.net"
LABEL description="zookeeper image for playing with kafka"
LABEL version=$VERSION

ENV ZOOK_VERSION=3.4.9

ADD files/zookeeper-${ZOOK_VERSION}.tar.gz /
RUN ln -s /zookeeper-${ZOOK_VERSION} /zookeeper

ADD etc /etc
ADD entrypoint.sh /

EXPOSE 2181

ENTRYPOINT /entrypoint.sh