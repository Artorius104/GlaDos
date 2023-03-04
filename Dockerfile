FROM ubuntu:20.04

COPY . /app/
WORKDIR /app

RUN apt-get update && apt-get install -y wget

RUN apt-get -y install llvm-9-dev curl libnuma-dev
RUN wget -qO- https://get.haskellstack.org/ | sh

RUN stack setup --resolver=lts-18.10 --install-ghc
RUN apt-get -y install make

CMD [ "/usr/bin/make ; ", "./glados", " test/test_0.scm/test_0.scm"]
