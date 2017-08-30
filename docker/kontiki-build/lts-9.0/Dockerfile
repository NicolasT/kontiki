FROM fpco/stack-build:lts-9.0
MAINTAINER Nicolas Trangez <ikke@nicolast.be>

ARG GRPC_TAG=v1.3.9

RUN apt-get update && \
    apt-get install -y libleveldb-dev && \
    rm -rf /var/lib/apt/lists/*

# The only GRPC packages for Ubuntu which are up-to-date are for Artful, in
# Universe, and they're utterly broken.
RUN mkdir -p /usr/src && \
    cd /usr/src && \
    git clone --recurse-submodules --branch ${GRPC_TAG} https://github.com/grpc/grpc.git && \
    cd grpc && \
    make -j2 prefix=/usr install_c && \
    cd .. && \
    rm -rf grpc
