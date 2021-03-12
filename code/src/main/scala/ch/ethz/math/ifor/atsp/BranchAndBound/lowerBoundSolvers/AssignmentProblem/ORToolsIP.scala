package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem

import ch.ethz.math.ifor.atsp.{Site, arcWise, inf}
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver
import com.google.ortools.linearsolver.MPConstraint
import com.google.ortools.linearsolver.MPObjective
import com.google.ortools.linearsolver.MPSolver
import com.google.ortools.linearsolver.MPVariable
import scala.collection.mutable

object ORToolsIP extends LowerBoundSolver{

  def compute(branchNode: BranchNode): (Map[Site, Map[Site, Boolean]],Map[Site, Map[Site, Double]])  = {

    System.loadLibrary("jniortools")
    /*
    var excluded: List[(Site,Site)] = List()
    var included: List[(Site,Site)] = List()
    branchNode.varAssignment.foreach{
      case (site1, map1) => (site1, map1.collect{
        case (site2, value) if value == Some(false) => excluded = excluded :+ (site1,site2)
        case (site2, value) if value == Some(true) => included = included :+ (site1,site2)
      })
    }
     */

    val solver: MPSolver = new MPSolver("AssignmentProblem",
      MPSolver.OptimizationProblemType.GLOP_LINEAR_PROGRAMMING)

    // x(i)(j) is an array of 0-1 variables, which will be 1
    // if site i is assigned to site j.
    //val x = Array.ofDim[MPVariable](numSites, numSites)

    def constructVariable(site1:Site ,site2:Site):MPVariable=
      if (site1.id==site2.id) {solver.makeIntVar(0,0,"")}
      else {
        //println("branchNode.varAssignment(site1)(site2)",branchNode.varAssignment(site1)(site2))
        branchNode.varAssignment(site1)(site2) match {
          case null => solver.makeIntVar(0,1,"")
          case Some(true)=> solver.makeIntVar(1,1,"")
          case Some(false)=> solver.makeIntVar(0,0,"")
        }
      }

    val x: arcWise[MPVariable] = arcWise(branchNode.input, constructVariable)

    val costs:arcWise[Double] = arcWise(branchNode.input,branchNode.input.distance)

//    for (i <- 0 until numSites) {
//      for (j <- 0 until numSites) {
//        if(i != j) {
//          //println("what happens",branchNode,branchNode.level,branchNode.sitesStatus(inputN.sites(i))(inputN.sites(j)))
//          x(i)(j) = if (branchNode.sitesStatus(branchNode.input.sites(i))(branchNode.input.sites(j)) == null) solver.makeIntVar(0, 1, "")
//          else if (branchNode.sitesStatus(branchNode.input.sites(i))(branchNode.input.sites(j)).contains(true)) solver.makeIntVar(1, 1, "")
//          else solver.makeIntVar(0, 0, "")
//        } else{
//          x(i)(j) = solver.makeIntVar(0, 0, "")
//        }
//        costs(i)(j) = branchNode.costsMap(branchNode.input.sites(i))(branchNode.input.sites(j))
//        //print(costs(i)(j)+"  ", i,j,inputN.sites(i),inputN.sites(j) )
//      }
//      //println("\r\n")
//    }

    /*
    // Each site has at most one out-degree.
    for (i <- branchNode.input.sites) {
      val constraint = solver.makeConstraint(1, 1, "")
      for (j <- branchNode.input.sites) {
        constraint.setCoefficient(x.search(i,j), 1)
        //println("Number of constraints = " + solver.numConstraints(),i,j,x.search(i,j))
      }
    }

    // Each site has at most one in-degree.
    for (j <- branchNode.input.sites) {
      val constraint = solver.makeConstraint(1, 1, "")
      for (i <- branchNode.input.sites) {
        constraint.setCoefficient(x.search(i,j), 1)
        //println("Number of constraints = " + solver.numConstraints(),i,j,x.search(i,j))
      }
    }

     */
    var constraintInMap: Map[Site,MPConstraint] = Map()
    var constraintOutMap: Map[Site,MPConstraint] = Map()

    //var listConstraintsIn: Map[MPConstraint,Double] = Map()
    //var listConstraintsOut: Map[MPConstraint,Double] = Map()
    //var constraints: Map[MPConstraint,Double] = Map()

    // construct in- & out-degree constraints
    for (site1 <- branchNode.input.sites){
      val constraintIn:MPConstraint = solver.makeConstraint(1, 1, "")
      val constraintOut:MPConstraint = solver.makeConstraint(1, 1, "")
      for (site2 <- branchNode.input.sites){
        constraintIn.setCoefficient(x.search(site1, site2),1)
        constraintOut.setCoefficient(x.search(site2, site1),1)
      }
      //listConstraintsIn = listConstraintsIn ++ Map(constraintIn->0.0)
      //listConstraintsOut = listConstraintsOut ++ Map(constraintOut->0.0)
      constraintInMap = constraintInMap ++ Map(site1->constraintIn)
      constraintOutMap = constraintOutMap ++ Map(site1->constraintOut)
    }
    //constraints = constraints ++ listConstraintsIn ++ listConstraintsOut

    // Create the objective function.
    val objective = solver.objective()
    for (i <- branchNode.input.sites) {
      for (j <- branchNode.input.sites) {
        objective.setCoefficient(x.search(i,j), costs.search(i,j))
      }
    }
    // println("num variables",solver.numVariables())
    objective.setMinimization()

    val resultStatus = solver.solve()
    //println(resultStatus)

    if(resultStatus == MPSolver.ResultStatus.INFEASIBLE){
      val assignmentInfeasible : Map[Site,Map[Site,Boolean]] = branchNode.varAssignment.map{
        case (site1, map1) => (site1, map1.map{
          case (site2, value) if site1==site2 => (site2,true)
          case (site2, value) if site1!=site2 => (site2,false)
        })
      }

      val costInfeasible : Map[Site,Map[Site,Double]] = branchNode.costsMap.map{
        case (site1, map1) => (site1, map1.map{
          case (site2, value) => (site2,inf)
        })
      }
      return (assignmentInfeasible,costInfeasible)
    }

    def constructResult(site1:Site ,site2:Site):Boolean= {
      if (x.search(site1,site2).solutionValue == 1) {
        //println(site1,site2,constraintInMap(site1).dualValue(),constraintOutMap(site2).dualValue(),
        //  branchNode.costsMap(site1)(site2)-constraintInMap(site1).dualValue()-constraintOutMap(site2).dualValue())
        true}
      else {false}
    }

    val resultArray: arcWise[Boolean] = arcWise(branchNode.input, constructResult)

    branchNode.costsMap.map{
      case (site1, map1) => (site1, map1.map{
        case (site2, value) => //println(site1,site2, value - constraintInMap(site1).dualValue() - constraintOutMap(site2).dualValue())
      })
    }

    //compute reduced cost
    val reducedCost = branchNode.costsMap.map{
      case (site1, map1) => (site1, map1.map{
        case (site2, value) => (site2, value - constraintInMap(site1).dualValue() - constraintOutMap(site2).dualValue())
      })
    }

    /*
    var reducedCost2 = mutable.Map[Site, Map[Site, Double]]()
    for (item <- branchNode.costsMap){
      reducedCost2 = reducedCost2 + item
    }


    println("stuck here reduced cost?")
    //reduced cost v2
    for(rowConstraint <- constraints){
      if (rowConstraint._1.dualValue()!=0.0){
        for(var1 <- x.entries){
          for(var2 <- var1._2){
            if (rowConstraint._1.getCoefficient(var2._2)!=0 && var1._1!=var2._1){
              //println("coeff before",var1._1,var2._1,reducedCost2(var1._1)(var2._1),rowConstraint.getCoefficient(var2._2),rowConstraint.dualValue())
              reducedCost2 = reducedCost2.map{
                case (site1, map1) => (site1, map1.map{
                  case (site2, value) if site1==var1._1 && site2==var2._1 => (site2, value -rowConstraint._1.dualValue())
                  case (site2, value) => (site2, value)
                })
              }

            }
          }
        }
      }
    }

     */

    /*
    println("===============reduced cost in OR Tools IP===================== ")
    reducedCost.collect{
      case (site1, map1) => map1.collect{
        case (site2, value) if value ==Double.NaN || value < 0=> println(site1,site2,value)
      }
    }

     */



/*

    println("===============reduced cost V2===================")
    reducedCost2.foreach{
      case (site1, map1) => map1.foreach{
        case (site2, value) => println(site1,site2,value)
      }
    }

 */



/*
    println("final matching")
    for(i<-resultArray.entries){
      for(j<-i._2){
        if (j._2){
          println(i._1.id,j._1.id,costs.entries(i._1)(j._1))
        }
      }
    }

 */
    //println("Finish OR Tools AP")
    (resultArray.entries,reducedCost)
    }

  // not used
  def computeLB (branchNode: BranchNode) : LowerBound = {
    0.0
  }
}