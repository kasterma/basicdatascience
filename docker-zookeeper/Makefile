VERSION=0.1.0
ZOOK_VERSION=3.4.9
ZOOK_URL=http://apache.cs.uu.nl/zookeeper/zookeeper-${ZOOK_VERSION}/zookeeper-${ZOOK_VERSION}.tar.gz

files/zookeeper-${ZOOK_VERSION}.tar.gz:
	mkdir -p files
	curl -o $@ ${ZOOK_URL}

build: files/zookeeper-3.4.9.tar.gz
	docker build --build-arg VERSION=${VERSION},ZOOK_VERSION=${ZOOK_VERSION} -t kasterma/zook-ke:${VERSION} .

ZOOK_HOSTNAME=zook-ke
ZOOK_NET=net-ke
run:
	-docker network create net-ke
	docker run --net=${ZOOK_NET} --name=${ZOOK_HOSTNAME} --hostname=${ZOOK_HOSTNAME} -d kasterma/zook-ke:${VERSION}
