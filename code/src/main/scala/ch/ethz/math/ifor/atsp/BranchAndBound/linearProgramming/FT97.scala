package ch.ethz.math.ifor.atsp.BranchAndBound.linearProgramming
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, linearProgrammingSolver}
import ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers.Patching.Karp79.computeUpperBound
import ch.ethz.math.ifor.atsp.{Site, inf}

import scala.collection.mutable

object FT97 extends LinearProgramming {
  def findSolution(branchNode: BranchNode): Map[Site, Map[Site, Double]] = {


    val solution = linearProgrammingSolver.findSolution(branchNode)

    branchNode.costsMap



  }

}
