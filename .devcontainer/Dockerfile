FROM ubuntu:latest

RUN apt-get update && apt-get install -y \
    sudo \
    lsb-release wget software-properties-common gnupg openssh-client \
    build-essential \
    python3 python3-pip \
    ;

# FIXME: Define these as features.

# Source: https://apt.llvm.org/
RUN bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"

ARG CMAKE_BIN_URL
RUN wget -qO- $CMAKE_BIN_URL \
    | tar --strip-components=1 -xz -C /usr/local

COPY --from=ghcr.io/sfu-rsl/z3_dist:4.11.2 /home/dist /usr

RUN useradd -m -s /bin/bash vscode \
    && echo 'ubuntu ALL=(ALL) NOPASSWD:ALL' > /etc/sudoers.d/ubuntu
USER vscode
