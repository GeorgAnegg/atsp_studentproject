#!/bin/bash

for f in ./src/main/resources/*.csv; do sbt "runMain ch.ethz.math.ifor.atsp.mainFiles.incremental ${f:21}"; done
