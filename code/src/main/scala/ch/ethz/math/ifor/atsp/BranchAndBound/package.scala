package ch.ethz.math.ifor.atsp

import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem.ORToolsIP
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver

package object BranchAndBound {

  val lowerBoundSolver:LowerBoundSolver = ORToolsIP
  type IsLeafNode = Boolean
  type LowerBound = Double
}
