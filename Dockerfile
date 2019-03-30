FROM    haskell:8.6.3 as compile
COPY    . /app
WORKDIR /app
RUN     stack build --resolver lts-13.4

FROM    debian:latest
RUN     mkdir -p /app
COPY    --from=compile /app/.stack-work/install/x86_64-osx/lts-13.4/8.6.3/bin/pixel /app/pixel
WORKDIR /app
CMD     ["/app/pixel", "run"]
