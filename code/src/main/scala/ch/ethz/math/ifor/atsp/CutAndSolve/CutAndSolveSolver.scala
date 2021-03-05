package ch.ethz.math.ifor.atsp.CutAndSolve

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.{Input, Output, Site, arcWise, inf, negInf}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

import scala.collection.mutable

object CutAndSolveSolver {

  def solve(input:Input,formulation:String):Unit = {

    System.loadLibrary("jniortools")

    val solverLP: MPSolver = new MPSolver("",
      MPSolver.OptimizationProblemType.GLOP_LINEAR_PROGRAMMING)

    // x(i)(j) is an array of 0-1 variables, which will be 1
    // if site i is assigned to site j.
    //val x = Array.ofDim[MPVariable](numSites, numSites)
    def constructVariable(site1:Site ,site2:Site):MPVariable= {
      if (site1.id==site2.id) {solverLP.makeNumVar(0,0,"")}
      else {
        solverLP.makeNumVar(0,1,"")
      }
    }

    val variables: arcWise[MPVariable] = arcWise(input, constructVariable)
    val costs:arcWise[Double] = arcWise(input,input.distance)
    var constraints: List[MPConstraint] = List()

    var listConstraintsIn: List[MPConstraint] = List()
    var listConstraintsOut: List[MPConstraint] = List()

    var constraintInMap: Map[Site,MPConstraint] = Map()
    var constraintOutMap: Map[Site,MPConstraint] = Map()

    // construct in- & out-degree constraints
    for (site1 <- input.sites){
      val constraintIn:MPConstraint = solverLP.makeConstraint(1, 1, "")
      val constraintOut:MPConstraint = solverLP.makeConstraint(1, 1, "")
      for (site2 <- input.sites){
        constraintIn.setCoefficient(variables.search(site1, site2),1)
        constraintOut.setCoefficient(variables.search(site2, site1),1)
      }
      listConstraintsIn = constraintIn::listConstraintsIn
      listConstraintsOut = constraintOut::listConstraintsOut
      constraintInMap = constraintInMap ++ Map(site1->constraintIn)
      constraintOutMap = constraintOutMap ++ Map(site1->constraintOut)
    }
    constraints = constraints ++ listConstraintsIn ++ listConstraintsOut

    // construct the objective function.
    val objectiveFunction : MPObjective = solverLP.objective()
    variables.entries.map{
      case (site1, map1) => (site1, map1.map{
        case (site2, variable) => objectiveFunction.setCoefficient(variable,costs.search(site1, site2))
      })
    }

    // println("num variables",solver.numVariables())
    objectiveFunction.setMinimization()

    val resultStatus = solverLP.solve()
    println("Result status: "+resultStatus)

    def constructResult(site1:Site ,site2:Site):Double= {
      variables.search(site1,site2).solutionValue
    }

    for (i <- input.sites) {
      for (j <- input.sites) {
        print("site: ",i, " to site: ",j," with cost ",variables.search(i,j).solutionValue+"\r\n")
      }
    }

    val currentLpSolution = arcWise(input, constructResult).entries

    //
    // initialize c_{ij}' = c_{ij}
    var reducedCost = mutable.Map[Site, Map[Site, Double]]()
    for (item <- input.distMat){
      reducedCost = reducedCost + item
    }

    val reducedCostV2 = reducedCost.map{
      case (site1, map1) => (site1,map1.map{
        case (site2, value) => (site2, value - constraintInMap(site1).dualValue() - constraintOutMap(site2).dualValue())
      })
    }
    reducedCost.foreach{
      case (site1, map1) => (site1,map1.foreach{
        case (site2, value) => println("V2",site1, site2, value, constraintInMap(site1).dualValue(), constraintOutMap(site2).dualValue())
      })
    }


    println("reduced cost V2")
    reducedCostV2.foreach{
      case (site1, map1) => map1.foreach{
        case (site2, value) => println(site1,site2,value)
      }
    }


    // update c_{ij}' row by row
    for(rowConstraint <- constraints){
      if (rowConstraint.dualValue()!=0.0){
        println(rowConstraint.dualValue())
        for(var1 <- variables.entries){
          for(var2 <- var1._2){
            if (rowConstraint.getCoefficient(var2._2)!=0 && var1._1!=var2._1){
              println("coef before",var1._1,var2._1,reducedCost(var1._1)(var2._1),rowConstraint.getCoefficient(var2._2),rowConstraint.dualValue())
              reducedCost = reducedCost.map{
                case (site1, map1) => (site1, map1.map{
                  case (site2, value) if site1==var1._1 && site2==var2._1 => (site2, value -rowConstraint.dualValue()*rowConstraint.getCoefficient(var2._2))
                  case (site2, value) => (site2, value)
                })
              }
                //reducedCost(var1._1).updated(var2._1,reducedCost(var1._1)(var2._1)-
                //rowConstraint.dualValue()*rowConstraint.getCoefficient(var2._2))
              println("coef after",var1._1,var2._1,reducedCost(var1._1)(var2._1))
            }
          }
        }
      }
    }

    println("reduced cost")
    reducedCost.foreach{
      case (site1, map1) => map1.foreach{
        case (site2, value) => println(site1,site2,value)
      }
    }



    // construct root node
    val numSites: Int = input.sites.length
    val initAssignmentArray: Array[Array[Option[Boolean]]] = Array.ofDim[Option[Boolean]](numSites, numSites)
    val initAssignmentMap: Map[Site, Map[Site, Option[Boolean]]] = input.sites.zip(initAssignmentArray).map { case (site, distRow) =>
      site -> input.sites.zip(distRow).toMap
    }.toMap

    var globalCuts: List[(Map[MPVariable,Double],Double)] = List()

    // solve LP relaxation and get an initial solution x
    //val rootNode: BranchNode = new BranchNode(input,initAssignmentMap,globalCuts,formulation)
    //val currentSolution = rootNode.lowerBoundSolve

    // generate a cut that cuts off a and another integer solution
    // this is done by first computing reduced cost


    // solve two sub problems, one on the sparse graph, on the dense graph

    // if LB >= current best, terminate; otherwise, proceed as above on the dense graph







  }

}
