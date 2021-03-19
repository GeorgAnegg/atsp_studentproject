package ch.ethz.math.ifor.atsp

import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem.{ORToolsIP,HungarianAP,HungarianV2,ParametricAP,ORToolsIPDual,HungarianTest,ParametricTest,ParamatricOriginal}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.Arborescence.{ChuLiuEdmonds,EdmondsTest,ChuLiuEdmondsV2,AdditiveV1,ChuLiuEdmondsTest}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.NaiveLowerBound.NaiveLB
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver
import ch.ethz.math.ifor.atsp.BranchAndBound.branchingSchemes.BranchingScheme
import ch.ethz.math.ifor.atsp.BranchAndBound.branchingSchemes.CT80
import ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers.Patching.{Karp79,Karp79V2}
import ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers.UpperBoundSolver


package object BranchAndBound {

  //val lowerBoundSolver: LowerBoundSolver = ORToolsIP
  val lowerBoundSolver: LowerBoundSolver = HungarianTest
  //val lowerBoundSolver: LowerBoundSolver = HungarianV2
  val naiveLowerBoundSolver: LowerBoundSolver = NaiveLB
  val rSAPLowerBoundSolver = ChuLiuEdmondsTest
  val branchingScheme: BranchingScheme = CT80
  val upperBoundSolver: UpperBoundSolver= Karp79V2
  val paramatricSolver: LowerBoundSolver = ParamatricOriginal

  type IsLeafNode = Boolean
  type LowerBound = Double

}
