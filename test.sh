#!/usr/bin/env bash

# A little wrapper for running the fat jar built with `lein uberjar`:

java -Djava.awt.headless=true -server -Djava.net.preferIPv4Stack=true --add-opens=java.base/java.nio=ALL-UNNAMED --add-opens=java.base/sun.nio.ch=ALL-UNNAMED -jar target/*-standalone.jar "$@"
