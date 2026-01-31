#!/bin/bash
set -e

PROJECT_ID="${PUBSUB_PROJECT_ID:-demo-hut}"
HOST_PORT="0.0.0.0:8086"

gcloud beta emulators pubsub start --host-port=${HOST_PORT} --project=${PROJECT_ID} &
PUBSUB_PID=$!

echo "Waiting for PubSub emulator to start..."
sleep 5
until curl -s "http://localhost:8086" > /dev/null 2>&1; do
  echo "Waiting..."
  sleep 2
done

echo "PubSub emulator is ready"

echo "Creating topic: events"
curl -s -X PUT "http://localhost:8086/v1/projects/${PROJECT_ID}/topics/events" || true

echo "Creating subscription: events-debug"
curl -s -X PUT "http://localhost:8086/v1/projects/${PROJECT_ID}/subscriptions/events-debug" \
  -H "Content-Type: application/json" \
  -d "{\"topic\":\"projects/${PROJECT_ID}/topics/events\"}" || true

echo "PubSub setup complete"
echo "  Topic: projects/${PROJECT_ID}/topics/events"
echo "  Subscription: projects/${PROJECT_ID}/subscriptions/events-debug"

wait $PUBSUB_PID
