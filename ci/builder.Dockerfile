FROM alpine:3.23

RUN apk add --no-cache \
  bash git build-base musl-dev gmp-dev m4 perl curl tar opam

RUN addgroup -S ocaml && adduser -S -G ocaml ocaml \
  && echo 'ocaml ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER ocaml
WORKDIR /home/ocaml

RUN opam init --disable-sandboxing -y \
  && opam update \
  && opam switch create musl \
    ocaml-variants.5.4.0+options ocaml-option-musl ocaml-option-static \
    --assume-depexts \
  && eval $(opam env)

WORKDIR /src
COPY --chown=opam:opam *.opam ./
RUN opam install . --deps-only -y

ENTRYPOINT ["opam", "exec", "--"]
CMD ["bash"]
