FROM kasterma/base-ke:0.1.2

ARG VERSION

LABEL maintainer="kasterma@kasterma.net"
LABEL description="kafka experimental image"
LABEL version=$VERSION

ARG KAFKA_VERSION

ADD files/kafka_${KAFKA_VERSION}.tgz /
RUN ln -s /kafka_${KAFKA_VERSION} /kafka

ADD etc /etc
ADD entrypoint.sh /

ENTRYPOINT /entrypoint.sh