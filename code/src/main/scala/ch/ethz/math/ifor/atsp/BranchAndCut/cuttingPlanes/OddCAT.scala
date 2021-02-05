package ch.ethz.math.ifor.atsp.BranchAndCut.cuttingPlanes

import ch.ethz.math.ifor.atsp.BranchAndCut.{BranchNode, BranchAndCutSolver}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

object OddCAT extends CuttingPlane {
  def findCuts(branchNode:BranchNode,globalCuts:List[(Map[MPVariable,Double],Double)]):List[(Map[MPVariable,Double],Double)] = {

    val thresholdSlackForSECs = 0.01
    val thresholdSlackForOthers = 0.1

    val maxNumberOfCuts = 20 + branchNode.input.sites.length/5

    // 1. first test the cuts in the global pool, if found, exit the separation procedure
    for (cut <- globalCuts){
      var eqValue:Double = 0.0
      for (variableMap <-cut._1){
        eqValue += variableMap._1.solutionValue()
      }
      if (eqValue>cut._2){
        return List(cut)
      }
    }

    // 2. otherwise, apply PR90a MINCUT algorithm for SEC separation, i.e., check if there
    // exists a subset of vertices such that x*(\delta(S))<2, if found, exit the separation procedure


    // 3. otherwise, shrink 1-arc paths of x* and apply separation algorithms for
    // comb, D_k and odd CAT


    // 3.1 for comb, first transform ATSP solution x* to TSP solution y* = x*_{ij} + x*_{ji}
    // then apply simplest heuristic separation scheme for comb (2-matching), then pull back to ATSP cuts


    // 4. otherwise, return an empty set







    List()

  }

}
