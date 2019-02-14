#!/bin/sh

# NOT INTENDED TO BE RUN LOCALLY
# This script is placed in the docker image as the entrypoint for the container.

# Start the redis server
redis-server --daemonize yes --protected-mode no

# Start the haskell server
server