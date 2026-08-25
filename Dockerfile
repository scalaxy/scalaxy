# syntax=docker/dockerfile:1
# Scalaxy -- multi-purpose cloud-ready distributed database (Common Lisp)
#
# Build:   docker build -t scalaxy .
# Run:     docker run -p 8080:8080 -p 7200:7200 scalaxy
#          (configuration via SCALAXY_* environment variables)

# ---- builder: compile everything and run the test suite ----
FROM debian:bookworm-slim AS builder

RUN apt-get update \
 && apt-get install -y --no-install-recommends sbcl \
 && rm -rf /var/lib/apt/lists/*

ENV HOME=/opt/scalaxy
WORKDIR /opt/scalaxy

COPY scalaxy.asd ./
COPY src/ src/
COPY tests/ tests/
COPY specs/ specs/
COPY scripts/ scripts/
COPY web/ web/

# Compile the systems and run the full test suite at build time so a
# broken build never produces an image.
RUN sbcl --non-interactive \
    --eval '(require :asdf)' \
    --eval '(asdf:load-asd (truename "scalaxy.asd"))' \
    --eval '(asdf:load-system "scalaxy")' \
    --eval '(asdf:load-system "scalaxy/tests")' \
    --eval '(sb-ext:exit :code (scalaxy-tests:run-all-tests))'

# ---- runtime ----
FROM debian:bookworm-slim AS runtime

RUN apt-get update \
 && apt-get install -y --no-install-recommends sbcl ca-certificates curl \
 && rm -rf /var/lib/apt/lists/* \
 && useradd --create-home --uid 1000 --shell /usr/sbin/nologin scalaxy \
 && mkdir -p /var/lib/scalaxy /opt/scalaxy \
 && chown -R scalaxy:scalaxy /var/lib/scalaxy /opt/scalaxy

ENV HOME=/home/scalaxy \
    SCALAXY_ADDRESS=0.0.0.0:7200 \
    SCALAXY_HTTP_ADDRESS=0.0.0.0:8080 \
    SCALAXY_DATA_DIR=/var/lib/scalaxy \
    SCALAXY_WEB_DIR=/opt/scalaxy/web/

WORKDIR /opt/scalaxy

COPY --from=builder /opt/scalaxy/ /opt/scalaxy/
COPY bin/ /opt/scalaxy/bin/
COPY scripts/ /opt/scalaxy/scripts/

USER scalaxy

EXPOSE 7200 8080

HEALTHCHECK --interval=15s --timeout=3s --start-period=5s --retries=3 \
  CMD curl -fsS http://127.0.0.1:8080/healthz >/dev/null || exit 1

ENTRYPOINT ["sbcl", "--script", "bin/run-node.lisp"]
