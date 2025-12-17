FROM alpine:3.23

RUN apk add --no-cache \
  bash git build-base musl-dev gmp-dev m4 perl curl tar opam github-cli

RUN addgroup -g 1001 gha && adduser -D -u 1001 -G gha gha \
  && echo 'gha ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER gha
ENV OPAMROOT=/home/gha/.opam
WORKDIR /home/gha

RUN opam init --disable-sandboxing -y \
  && opam update \
  && opam switch create musl \
    ocaml-variants.5.4.0+options ocaml-option-musl \
    --assume-depexts \
  && opam switch set musl

WORKDIR /src
COPY --chown=opam:opam *.opam ./
RUN opam install . --deps-only -y

ENTRYPOINT ["opam", "exec", "--"]
CMD ["bash"]
