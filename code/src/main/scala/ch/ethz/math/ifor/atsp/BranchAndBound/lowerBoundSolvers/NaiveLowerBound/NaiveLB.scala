package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.NaiveLowerBound
import ch.ethz.math.ifor.atsp.{Site, inf}
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver

import scala.util.control.Breaks.{break, breakable}

object NaiveLB extends LowerBoundSolver{

  def compute (branchNode: BranchNode) : Map[Site, Map[Site, Boolean]] = {
    branchNode.lowerBoundSolve
  }

  def computeLB(branchNode: BranchNode): LowerBound  ={
    var resultLB: LowerBound = 0.0

    val numSites = branchNode.sitesStatus.size
    val inputN = branchNode.inputNode
    val costs = Array.ofDim[Double](numSites, numSites)

    for (i <- 0 until numSites) {
      for (j <- 0 until numSites) {
        if (i == j){
          costs(i)(j) = inf
        } else {
          costs(i)(j) = branchNode.costsMap(inputN.sites(i))(inputN.sites(j))
        }
      }
    }

    for (map1 <- branchNode.sitesStatus){
      for (map2 <- map1._2){
        if (map2._2 != null) {
          if (map2._2.get) {
            val index1 = inputN.sites.indexOf(map1._1)
            val index2 = inputN.sites.indexOf(map2._1)
            for(i <- 0 until numSites){
              costs(index1)(i) = inf // block the row
              costs(i)(index2) = inf // block the column
            }
          }
          else costs(inputN.sites.indexOf(map1._1))(inputN.sites.indexOf(map2._1)) = inf // block one block
        }
      }
    }
    for (i <- 0 until numSites){
      val min = costs(i).min
      costs(i).foreach(x=>x-min)
      resultLB += min
    }

    def getColumn(matrix:Array[Array[Double]],index:Int):Array[Double]={
      val result: Array[Double] = Array.fill(matrix.length)(0.0)
      for (i <- matrix.indices){
        result(i) = matrix(i)(index)
      }
      result
    }


    for (j <- 0 until numSites){
      val min = getColumn(costs,j).min
      resultLB += min
    }
    println("resultlbb",resultLB)
  resultLB
  }
}
