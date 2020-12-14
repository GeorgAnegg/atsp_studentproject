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
    /*
    val numSites = branchNode.varAssignment.size
    val inputN = branchNode.input
    var costs = Array.ofDim[Double](numSites, numSites)

    if (branchNode.parentNode != branchNode){
      resultLB = branchNode.parentNode.naiveLowerBound
      costs = branchNode.parentNode.reducedCostMatrix
    } else {
      for (i <- 0 until numSites) {
        for (j <- 0 until numSites) {
          if (i == j){
            costs(i)(j) = inf
          } else {
            costs(i)(j) = branchNode.costsMap(inputN.sites(i))(inputN.sites(j))
          }
        }
      }
    }
    for (map1 <- branchNode.varAssignment){
      for (map2 <- map1._2){
        if (map2._2 != null) {
          if (map2._2.get) {
            val index1 = inputN.sites.indexOf(map1._1)
            val index2 = inputN.sites.indexOf(map2._1)
            for(i <- 0 until numSites){
              costs(index1)(i) = inf // block the row
              costs(i)(index2) = inf // block the column
              //println("here we block the whole", map1._1,map2._1)
            }
          }
          else {
            costs(inputN.sites.indexOf(map1._1))(inputN.sites.indexOf(map2._1)) = inf // block one block
            //println("here we block one", map1._1, map2._1)
          }
        }
      }
    }
    for (i <- 0 until numSites){
      val min = costs(i).min
      //println("min row",min)
      if (min != inf) {
        costs(i) = costs(i).map(x=>x-min)
        resultLB += min
      }
    }
    def getColumn(matrix:Array[Array[Double]],index:Int):Array[Double]={
      val result: Array[Double] = Array.fill(matrix.length)(0.0)
      for (i <- matrix.indices){
        result(i) = matrix(i)(index)
      }
      result
    }
    def substractColumn(matrix:Array[Array[Double]],index:Int,value:Double):Unit={
      for (i <- matrix.indices){
        matrix(i)(index) = matrix(i)(index) - value
      }
    }
    for (j <- 0 until numSites){
      val min = getColumn(costs,j).min
      //println("min col",min)
      if (min != inf) {
        resultLB += min
        substractColumn(costs,j,min)
      }
    }
    // update the reduced cost matrix
    branchNode.reducedCostMatrix = costs
    println("resultlbb",resultLB)

     */
  resultLB
  }
}
