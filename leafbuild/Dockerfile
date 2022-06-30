# SPDX-License-Identifier
# Copyright (C) 2021-2022 Simon Fraser University (www.sfu.ca)

#
# Set up Ubuntu environment
#
FROM ubuntu:22.04 AS builder_base

SHELL ["/bin/bash", "-c"]

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y \
        sudo \
    && apt-get clean

RUN useradd -m -s /bin/bash ubuntu \
    && echo 'ubuntu ALL=(ALL) NOPASSWD:ALL' > /etc/sudoers.d/ubuntu

USER ubuntu
ENV HOME=/home/ubuntu
WORKDIR $HOME

#
# Set up project source
#
FROM builder_base AS builder_source

ENV LEAF_LLVM_VERSION=14

RUN sudo apt-get update \
    && sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
        clang-$LEAF_LLVM_VERSION \
        curl \
        cmake \
        g++ \
        python3-setuptools \
    && sudo apt-get clean

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh /dev/stdin -y

ENV PATH=$HOME/.cargo/bin:$PATH

ARG LEAF_RUST_VERSION
RUN rustup default "$LEAF_RUST_VERSION"
RUN rustup component add rust-src rustc-dev llvm-tools-preview

COPY --chown=ubuntu:ubuntu . leaf

#
# Build leaf
#
FROM builder_source AS builder_leaf

RUN cd leaf \
    && cargo build
