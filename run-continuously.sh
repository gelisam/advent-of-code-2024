#!/bin/bash
set -e

./run.sh
fswatcher --throttle 300 --path toy.rkt -- ./run.sh
