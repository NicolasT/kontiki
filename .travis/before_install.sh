#!/bin/bash -xue

GRPC_TAG=v1.3.9
GRPC_PREFIX=$HOME/.local

mkdir -p $HOME/.local/bin

# Install Stack
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C $HOME/.local/bin '*/stack'
chmod a+x $HOME/.local/bin/stack

# Install GRPC
if ! [ -f ${GRPC_PREFIX}/lib/libgrpc_unsecure.so ]; then
        pushd /tmp
        git clone --recurse-submodules --branch ${GRPC_TAG} https://github.com/grpc/grpc.git
        pushd grpc
        make -j2 prefix=${GRPC_PREFIX} install_c
        popd
        popd
fi

# Configure Stack
cat >> ~/.stack/config.yaml << EOF
extra-include-dirs:
- ${GRPC_PREFIX}/include
extra-lib-dirs:
- ${GRPC_PREFIX}/lib
EOF

cat ~/.stack/config.yaml
