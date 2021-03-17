#!/bin/bash

export SCALA_INTERPRETER=/u/ganegg/Downloads/scala-2.13.3/scala-2.13.3/bin/scala
export SCALA_RUNTIME_LIBRARY=/u/ganegg/Downloads/scala-2.13.3/scala-2.13.3/lib/scala-library.jar
export ASSEMBLY=/u/ganegg/Documents/atsp/code/target/scala-2.13/code-assembly-0.1.0-SNAPSHOT.jar

$SCALA_INTERPRETER -J-Djava.library.path=$OR_TOOLS_HOME/lib -cp $ASSEMBLY:$SCALA_RUNTIME_LIBRARY instAlgoIncremental.scala $1 $2
