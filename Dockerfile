FROM    haskell:8.6.3 as depends
WORKDIR /app
COPY    stack.yaml package.yaml /app/
RUN     stack build --resolver lts-13.4 --only-dependencies


FROM    haskell:8.6.3 as compile
WORKDIR /app
COPY    . /app
COPY    --from=depends /app/.stack-work /app/.stack-work
COPY    --from=depends /root/.stack /root/.stack
RUN     stack build --resolver lts-13.4
RUN     mv $(stack path --local-install-root)/bin/pixel .


FROM    debian:latest
RUN     apt-get -y update && apt-get -y install libgmp-dev
RUN     mkdir -p /app
WORKDIR /app
COPY    --from=compile /app/pixel .
CMD     ["/app/pixel", "run"]
