# HACK: For builds on master just throw the compiled binary into a Docker
# image.
FROM    haskell:8.6.3 as depends
WORKDIR /app
COPY    stack.yaml package.yaml /app/
COPY    .stack-work .stack-work
RUN     mv $(stack path --no-install-ghc --skip-ghc-check --local-install-root)/bin/pixel .

# Stripe out everything and throw the binary into a slim Alpine image.
FROM    frolvlad/alpine-glibc:latest
WORKDIR /app
RUN     apk update && apk add gmp
COPY    --from=depends /app/pixel .
CMD     ["/app/pixel", "run", "--address", "localhost"]
