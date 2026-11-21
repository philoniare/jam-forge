#!/usr/bin/env bash
set -euo pipefail

HOST="$1"
REF=c70230a58ffbe3c72a83447aaab19533eaa221c9
DIR=/opt/tart
NODE_ENV=/etc/jam-node/env
LOG=/var/log/jam-node/node.log
NODE_FLAGS=(--telemetry 127.0.0.1:9000 --rpc-port 19800 --rpc-listen-ip 127.0.0.1 --rpc-methods safe)

cloud-init status --wait >/dev/null 2>&1 || true
if ! command -v git >/dev/null || ! docker compose version >/dev/null 2>&1 || ! docker buildx version >/dev/null 2>&1; then
  apt-get update -q && DEBIAN_FRONTEND=noninteractive apt-get install -yq git docker-compose-v2 docker-buildx
fi

mkdir -p "$DIR"
[[ -d "$DIR/jamtart/.git" ]] || git clone -q https://github.com/paritytech/jamtart "$DIR/jamtart"
git -C "$DIR/jamtart" fetch -q origin "$REF"
git -C "$DIR/jamtart" checkout -q --detach "$REF"

PASSWORD=$(sed -n 's/^POSTGRES_PASSWORD=//p' "$DIR/.env" 2>/dev/null || true)
[[ -n "$PASSWORD" ]] || PASSWORD=$(openssl rand -hex 24)
(umask 077; printf 'POSTGRES_PASSWORD=%s\nTART_HOST=%s\nJAMTART_REF=%s\n' "$PASSWORD" "$HOST" "$REF" > "$DIR/.env")

cat > "$DIR/docker-compose.yml" <<'EOF'
name: tart
services:
  postgres:
    image: timescale/timescaledb:latest-pg17
    restart: unless-stopped
    shm_size: 1gb
    mem_limit: 3g
    environment:
      POSTGRES_USER: tart
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD:?}
      POSTGRES_DB: tart_telemetry
      POSTGRES_INITDB_ARGS: -E UTF8 --locale=C
    ports: ["127.0.0.1:5432:5432"]
    volumes: [postgres-data:/var/lib/postgresql/data]
    command: [postgres, -c, shared_buffers=1GB, -c, effective_cache_size=3GB, -c, work_mem=16MB,
              -c, maintenance_work_mem=256MB, -c, max_connections=100, -c, min_wal_size=512MB,
              -c, max_wal_size=2GB, -c, synchronous_commit=off, -c, statement_timeout=30000,
              -c, idle_in_transaction_session_timeout=30000, -c, jit=off]
    healthcheck:
      test: [CMD-SHELL, pg_isready -U tart -d tart_telemetry]
      interval: 30s
      timeout: 5s
      retries: 5
      start_period: 10s

  backend:
    build: ./jamtart
    image: tart-backend:${JAMTART_REF:?}
    restart: unless-stopped
    network_mode: host
    mem_limit: 2g
    environment:
      DATABASE_URL: postgres://tart:${POSTGRES_PASSWORD}@127.0.0.1:5432/tart_telemetry
      TELEMETRY_BIND: 127.0.0.1:9000
      API_BIND: 127.0.0.1:8080
      JAM_RPC_URL: ws://127.0.0.1:19800
      INGESTION_THREADS: "2"
      RUST_LOG: info
    depends_on:
      postgres: {condition: service_healthy}

  caddy:
    image: caddy:2.10.2
    restart: unless-stopped
    network_mode: host
    environment:
      TART_HOST: ${TART_HOST:?}
    volumes: [./Caddyfile:/etc/caddy/Caddyfile:ro, caddy-data:/data, caddy-config:/config]
    depends_on: [backend]

volumes:
  postgres-data:
  caddy-data:
  caddy-config:
EOF

cat > "$DIR/Caddyfile" <<'EOF'
{$TART_HOST} {
	handle /api/* {
		@write not method GET HEAD OPTIONS
		respond @write 405
		reverse_proxy 127.0.0.1:8080
	}
	handle {
		respond "TART backend of the JAM-Forge testnet node. Open https://tart.jamtoaster.network and set its backend URL to https://{host}" 200
	}
}
EOF

mkdir -p /etc/systemd/system/jam-node.service.d
cat > /etc/systemd/system/jam-node.service.d/tart.conf <<'EOF'
[Service]
ExecStartPost=-/bin/bash -c 'for i in $$(seq 90); do (exec 3<>/dev/tcp/127.0.0.1/19800) 2>/dev/null && break; sleep 2; done; docker restart tart-backend-1 >/dev/null'
EOF
systemctl daemon-reload

cd "$DIR"
docker compose up -d --build --quiet-pull
for _ in $(seq 60); do curl -fsS http://127.0.0.1:8080/api/health >/dev/null 2>&1 && break; sleep 5; done
curl -fsS http://127.0.0.1:8080/api/health; echo

args=$(sed -n 's/^JAM_ARGS=//p' "$NODE_ENV")
add=""
for ((i = 0; i < ${#NODE_FLAGS[@]}; i += 2)); do
  [[ " $args " == *" ${NODE_FLAGS[i]} "* ]] || add+=" ${NODE_FLAGS[i]} ${NODE_FLAGS[i + 1]}"
done
if [[ -n "$add" ]]; then
  sed -i "s|^JAM_ARGS=.*|JAM_ARGS=$args$add|" "$NODE_ENV"
  if [[ -n "$(sed -n 's/^JAM_IMAGE=//p' "$NODE_ENV")" ]]; then
    since=$(date -u +%Y-%m-%dT%H:%M:%S)
    systemctl restart jam-node
    for _ in $(seq 24); do
      sleep 5
      awk -v s="$since" '$1 >= s' "$LOG" | grep -q 'telemetry connected' && break
    done
    awk -v s="$since" '$1 >= s' "$LOG" | grep -E 'telemetry|RPC' || echo "no telemetry line in the node log yet" >&2
  fi
elif (exec 3<>/dev/tcp/127.0.0.1/19800) 2>/dev/null; then
  docker restart tart-backend-1 >/dev/null
fi
for _ in $(seq 12); do curl -fsS -o /dev/null http://127.0.0.1:8080/api/jam/stats 2>/dev/null && break; sleep 5; done
curl -sS -o /dev/null -w '/api/jam/stats %{http_code}\n' http://127.0.0.1:8080/api/jam/stats
echo "tart ready at https://$HOST"
