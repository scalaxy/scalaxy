#!/bin/sh
set -eu
compose="docker compose -f docker-compose.garage.yml"
$compose up -d
trap '$compose down -v' EXIT
sleep 2
node_id="$($compose exec -T garage /garage status | awk '/^[[:space:]]*[0-9a-f]/{print $1; exit}')"
$compose exec -T garage /garage layout assign "$node_id" -z dc1 -c 1G
$compose exec -T garage /garage layout apply --version 1
key_info="$($compose exec -T garage /garage key create scalaxy-test)"
access_key="$(printf '%s\n' "$key_info" | awk '/Key ID/{print $NF}')"
secret_key="$(printf '%s\n' "$key_info" | awk '/Secret key/{print $NF}')"
$compose exec -T garage /garage bucket create scalaxy-test
$compose exec -T garage /garage bucket allow scalaxy-test --key scalaxy-test --read --write --owner
SCALAXY_S3_ENDPOINT="${SCALAXY_S3_ENDPOINT:-http://127.0.0.1:3900}" \
SCALAXY_S3_BUCKET=scalaxy-test SCALAXY_S3_ACCESS_KEY="$access_key" \
SCALAXY_S3_SECRET_KEY="$secret_key" sbcl --script scripts/test-s3.lisp
