package ch.ethz.math.ifor.atsp.BranchAndCut

import ch.ethz.math.ifor.atsp.{Input, Site, Tour}
import optimus.optimization.model.MPConstraint

class BranchNode(val input: Input,
                 var varAssignment: Map[Site, Map[Site, Option[Boolean]]]
                ) {
  var cuts:List[MPConstraint] = List()
  var reducedCosts:Map[Site, Map[Site, Double]]=Map()
  val isInteger:Boolean = false

  var level = 0
  val costsMap: Map[Site, Map[Site, Double]] = input.distMat
  val lowerBoundSolve: Map[Site, Map[Site, Double]] =linearProgrammingSolver.findSolution(branchNode = this)

  var parentNode: BranchNode = this
  //var reducedCostMatrix: Map[Site, Map[Site, Double]] = Map()
  //val naiveLowerBound: LowerBound = naiveLowerBoundSolver.computeLB(branchNode = this)
  val lowerBound: Double  = 0.0 // TODO


}
