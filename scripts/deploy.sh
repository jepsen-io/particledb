#!/bin/bash

# A quick helper for standing up a single ParticleDB instance of the given
# local binary on `n1`.

ssh n1 bash -c "'killall -9 particledb; mkdir -p /opt/particledb'" &&
scp $1 n1:/opt/particledb/particledb &&
ssh n1 bash -c "'cd /opt/particledb; \
  rm -f daemon.log; \
  start-stop-daemon --start \
  --chdir /opt/particledb \
  --background \
  --output /opt/particledb/daemon.log \
  --exec particledb \
  -- \
  start \
  --data-dir /opt/particledb/data \
  --pg-addr 0.0.0.0:15432 \
  --txn-mode occ-v3 \
  --no-auth \
  ; \
  while ! echo exit | nc -z localhost 15432; do sleep 0.5; done;
  '" &&
ssh n1 -t psql --host localhost -p 15432
