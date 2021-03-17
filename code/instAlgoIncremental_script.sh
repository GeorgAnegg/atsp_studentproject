#!/bin/bash

export SCALA_INTERPRETER=/home/ganegg/tmp/scala-2.13.3/scala-2.13.3/bin/scala
export SCALA_RUNTIME_LIBRARY=/home/ganegg/tmp/scala-2.13.3/scala-2.13.3/lib/scala-library.jar
export ASSEMBLY=/home/ganegg/atsp/code/target/scala-2.13/code-assembly-0.1.0-SNAPSHOT.jar

SOLVERS="CDT FT92 FT97 MTZ_FT97" # MTZ GG DL"
INSTANCES="br17.csv ft70.csv ftv35.csv ftv47.csv  ftv70.csv    rbg323.csv  rbg443.csv  ftv170.csv  ftv38.csv  ftv55.csv  kro124p.csv  rbg358.csv ry48p.csv ft53.csv           ftv33.csv   ftv44.csv  ftv64.csv  p43.csv      rbg403.csv"

# doesn't do anything JAVA_OPTS="-Djava.library.path=$OR_TOOLS_HOME/lib"

#$OR_TOOLS_HOME=/u/ganegg/Downloads

for instance in $INSTANCES; do
	for solver in $SOLVERS; do
		echo starting timeout on solving $instance with $solver
		timeout 300 ./helper.sh $instance $solver
		#sleep 3
		#export solver
		#export instance
		#timeout --foreground -k 1 3 bash -c '$SCALA_INTERPRETER -J-Djava.library.path=$OR_TOOLS_HOME/lib -cp $ASSEMBLY:$SCALA_RUNTIME_LIBRARY instAlgoIncremental.scala $instance $solver'
		#$SCALA_INTERPRETER -J-Djava.library.path=$OR_TOOLS_HOME/lib -cp $ASSEMBLY:$SCALA_RUNTIME_LIBRARY instAlgoIncremental.scala $instance $solver
	done
done



