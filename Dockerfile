FROM ocaml/opam:debian-11 AS builder

RUN sudo apt-get update && sudo apt-get install -y \
    m4 \
    pkg-config \
    git \
    unzip \
    --no-install-recommends

RUN opam init --disable-sandboxing --reinit -ni
RUN opam update

RUN opam install \
    alcotest \
    ocamlformat \
    ocaml-lsp-server \
    odoc \
    dune

FROM ocaml/opam:debian-11

RUN sudo apt-get update && sudo apt-get install -y \
    git \
    --no-install-recommends && \
    sudo rm -rf /var/lib/apt/lists/*

COPY --from=builder /home/opam/.opam /home/opam/.opam

ENV PATH="/home/opam/.opam/default/bin:${PATH}" \
    OPAMROOT="/home/opam/.opam" \
    OPAMSWITCH="default"

RUN echo ". /home/opam/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" >> /home/opam/.bashrc

WORKDIR /home/opam/app

USER opam

CMD [ "bash" ]