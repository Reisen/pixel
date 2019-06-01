# Build Haskell Dependencies Seperately
# This allows creating a layer containing all the libraries required to build
# cached between source changes.
FROM    haskell:8.6.3 as depends
WORKDIR /app
COPY    stack.yaml package.yaml /app/
RUN     stack build --resolver lts-13.4 --only-dependencies -j 2


# Build Haskell Binary Itself
# This allows creating a layer containing just the binary required to run the
# application, which won't change if the src doesn't change.
FROM    haskell:8.6.3 as compile
RUN     mkdir /app
WORKDIR /app
COPY    src/ /app/src
COPY    stack.yaml package.yaml /app/
COPY    --from=depends /app/.stack-work /app/.stack-work
COPY    --from=depends /root/.stack /root/.stack
RUN     stack build --resolver lts-13.4
RUN     mv $(stack path --local-install-root)/bin/pixel .


# Build Web Frontend
FROM    node:slim as web
RUN     apt-get -y update && apt-get -y install file unzip curl
WORKDIR /app
COPY    web/ /app/web
RUN     cd /app/web && yarn install && yarn build


# Build Final Image
FROM    debian:latest
RUN     apt-get -y update && apt-get -y install libgmp-dev
RUN     mkdir -p /app
WORKDIR /app
COPY    --from=compile /app/pixel .
COPY    --from=web /app/web/build /app/web
CMD     ["/app/pixel", "run"]
