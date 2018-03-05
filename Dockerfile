FROM alpine:3.6 as builder
MAINTAINER github@erebe.eu

RUN apk --no-cache add --repository http://dl-cdn.alpinelinux.org/alpine/edge/community \
        ca-certificates git ghc upx curl musl-dev gmp-dev zlib-dev pcre-dev
RUN curl -sSL https://get.haskellstack.org/ | sh

COPY stack.yaml /mnt
COPY *.cabal /mnt
WORKDIR /mnt
RUN rm -rf ~/.stack &&  \
    stack config set system-ghc --global true && \
    stack setup && \
    stack install --split-objs --ghc-options="-fPIC -fllvm" --only-dependencies

COPY . /mnt

RUN stack install --split-objs --ghc-options="-fPIC -fllvm"
RUN upx --ultra-brute /root/.local/bin/hmailclassifier



FROM alpine:latest as runner
MAINTAINER github@erebe.eu

WORKDIR /root
COPY --from=builder /root/.local/bin/hmailclassifier .
RUN chmod +x ./hmailclassifier

ENTRYPOINT ["./hmailclassifier"]

