FROM haskell:8.6@sha256:ef523a468575a880630cf7b5487862dd22641991a6615e1272d36309dbeb7f03

WORKDIR /temp

RUN stack update

ADD custom-snapshot.yaml /temp/custom-snapshot.yaml
ADD stack.yaml /temp/stack.yaml

ADD package.yaml /temp/package.yaml

RUN stack build --only-dependencies

RUN stack test --only-dependencies
