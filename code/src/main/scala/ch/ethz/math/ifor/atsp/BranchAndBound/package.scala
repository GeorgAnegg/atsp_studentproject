package ch.ethz.math.ifor.atsp

import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem.ORToolsIP
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem.HungarianAP
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.Arborescence.ChuLiuEdmonds
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.NaiveLowerBound.NaiveLB
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver
import ch.ethz.math.ifor.atsp.BranchAndBound.branchingSchemes.BranchingScheme
import ch.ethz.math.ifor.atsp.BranchAndBound.branchingSchemes.CT80
import ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers.Patching.Karp79
import ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers.UpperBoundSolver


package object BranchAndBound {

  val lowerBoundSolver: LowerBoundSolver = ORToolsIP
  //val lowerBoundSolver: LowerBoundSolver = HungarianAP
  val naiveLowerBoundSolver: LowerBoundSolver = NaiveLB
  val rSAPLowerBoundSolver = ChuLiuEdmonds
  val branchingScheme: BranchingScheme = CT80
  val upperBoundSolver: UpperBoundSolver= Karp79
  type IsLeafNode = Boolean
  type LowerBound = Double

}
