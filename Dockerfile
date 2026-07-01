FROM rust:1.96

RUN apt-get update && apt-get install -y \
    lsb-release

RUN bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"