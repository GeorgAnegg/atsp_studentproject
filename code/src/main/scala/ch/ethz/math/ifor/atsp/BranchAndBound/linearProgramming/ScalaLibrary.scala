package ch.ethz.math.ifor.atsp.BranchAndBound.linearProgramming
import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers.Patching.Karp79.computeUpperBound
import ch.ethz.math.ifor.atsp.{Site, arcWise, inf}
import com.google.ortools.linearsolver.MPVariable
import optimus.algebra.Expression

import scala.collection.mutable
import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar

object ScalaLibrary {
  def findSolution(branchNode: BranchNode): Map[Site, Map[Site, Double]] = {

    implicit val model = MPModel(SolverLib.oJSolver)

    // construct MPFloatVar variables
    def constructVariable(site1:Site ,site2:Site):MPFloatVar=
      if (site1.id==site2.id) {MPFloatVar("", 0, 0)}
      else {
        //println("branchNode.varAssignment(site1)(site2)",branchNode.varAssignment(site1)(site2))
        branchNode.varAssignment(site1)(site2) match {
          case null => MPFloatVar("", 0, 1)
          case Some(true)=> MPFloatVar("", 1, 1)
          case Some(false)=> MPFloatVar("", 0, 0)
        }
      }

    val vars: arcWise[MPFloatVar] = arcWise(branchNode.input, constructVariable)
    val costs:arcWise[Double] = arcWise(branchNode.input,branchNode.input.distance)

    // construct in- & out-degree constraints
    for (i <- branchNode.input.sites) {
      var listVarOut:List[MPFloatVar] = List()
      var listVarIn:List[MPFloatVar] = List()
      for (j <- branchNode.input.sites) {
        listVarOut = listVarOut :+ vars.search(i,j)
        listVarIn = listVarIn :+ vars.search(j,i)
      }
      var expOut = listVarOut.head + listVarOut(1)
      var expIn = listVarIn.head + listVarIn(1)
      for (i <- 2 to listVarOut.length){
        expOut = expOut + listVarOut(i)
      }
      for (i <- 2 to listVarIn.length){
        expIn = expIn + listVarIn(i)
      }
      add(expOut := 1)
      add(expIn := 1)
    }

    // construct objective function
    var expObjective = costs.search(branchNode.input.sites.head,
      branchNode.input.sites.head) * vars.search(branchNode.input.sites.head,
      branchNode.input.sites.head) + costs.search(branchNode.input.sites(1),
      branchNode.input.sites(1)) * vars.search(branchNode.input.sites(1),
      branchNode.input.sites(1))

    for (i<-branchNode.input.sites){
      for (j <-branchNode.input.sites){
        expObjective = expObjective + costs.search(i,j) * vars.search(i,j)
      }
    }

    maximize(expObjective)

    start()

    println(s"objective: $objectiveValue")
    //println(s"x = ${x.value} y = ${y.value}")

    release()
    branchNode.costsMap

    }









}
