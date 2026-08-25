#!/bin/sh
set -eu
cd "$(dirname "$0")"
compose="docker compose -f docker-compose.yml"

echo ">> starting garage..."
$compose up -d garage
sleep 3

node_id="$($compose exec -T garage /garage status | awk '/^[[:space:]]*[0-9a-f]/{print $1; exit}')"
$compose exec -T garage /garage layout assign "$node_id" -z dc1 -c 1G >/dev/null 2>&1 || true
$compose exec -T garage /garage layout apply --version 1 >/dev/null 2>&1 || true

$compose exec -T garage /garage bucket create chat >/dev/null 2>&1 || true

# fresh key each run; parse the SECRET from create output (key info hides it)
out="$($compose exec -T garage /garage key create chat-demo-$(date +%s))"
access_key="$(printf '%s\n' "$out" | awk '/Key ID/{print $NF}')"
secret_key="$(printf '%s\n' "$out" | awk '/Secret key/{print $NF}')"
$compose exec -T garage /garage bucket allow chat --key "$access_key" --read --write --owner >/dev/null

cat > .env <<EOF
CHAT_S3_ACCESS_KEY=$access_key
CHAT_S3_SECRET_KEY=$secret_key
EOF
echo ">> .env written with key ${access_key}"
