#!/bin/bash

export SCALA_INTERPRETER=/home/ganegg/tmp/scala-2.13.3/bin/scala
export SCALA_RUNTIME_LIBRARY=/home/ganegg/tmp/scala-2.13.3/lib/scala-library.jar
export ASSEMBLY=/home/ganegg/atsp/code/target/scala-2.13/code-assembly-0.1.0-SNAPSHOT.jar

$SCALA_INTERPRETER -J-Djava.library.path=$OR_TOOLS_HOME/lib -J-Xmx8g -cp $ASSEMBLY:$SCALA_RUNTIME_LIBRARY instAlgoIncremental.scala $1 $2
