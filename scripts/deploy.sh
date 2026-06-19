#!/bin/bash

# A quick helper for standing up a single ParticleDB instance of the given
# local binary on `n9`.

HOST=n9

ssh $HOST bash -c "'killall -9 particledb; mkdir -p /opt/particledb'" &&
scp $1 $HOST:/opt/particledb/particledb &&
ssh $HOST bash -c "'cd /opt/particledb; \
  rm -rf daemon.log data; \
  chmod +x particledb \
  start-stop-daemon --start \
  --chdir /opt/particledb \
  --background \
  --output /opt/particledb/daemon.log \
  --exec particledb \
  -- \
  start \
  --data-dir /opt/particledb/data \
  --pg-addr 0.0.0.0:15432 \
  --no-auth \
  --txn-mode occ-v3 \
  ; \
  while ! echo exit | nc -z localhost 15432; do sleep 0.5; done;
  '" &&
ssh $HOST -t psql --host localhost -p 15432
#  --pg-username jepsenuser \
#  --pg-password jepsenpw \
