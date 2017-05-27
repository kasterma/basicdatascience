VERSION=0.1.0
KAFKA_VERSION=0.10.2.1
KAFKA_FULL_VERSION=2.11-${KAFKA_VERSION}
KAFKA_URL=http://apache.mirror1.spango.com/kafka/${KAFKA_VERSION}/kafka_${KAFKA_FULL_VERSION}.tgz

files/kafka_${KAFKA_FULL_VERSION}.tgz:
	mkdir -p files
	curl -o $@ ${KAFKA_URL}

build: files/kafka_${KAFKA_FULL_VERSION}.tgz
	docker build --build-arg VERSION=${VERSION} --build-arg KAFKA_VERSION=${KAFKA_FULL_VERSION} -t kasterma/kafka-ke:${VERSION} .

run:
	docker run -e ZOOKEEPER_CONNECT=zook-ke:2181 --net=net-ke -d kasterma/kafka-ke:${VERSION}
