package ch.ethz.math.ifor.atsp.BranchAndBound

import ch.ethz.math.ifor.atsp.Site
import com.google.ortools.linearsolver.MPVariable

package object lowerBoundSolvers{

  val variables: Map[Site, Map[Site, MPVariable]] = ???

}