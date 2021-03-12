package ch.ethz.math.ifor.atsp.BranchAndCut.pricingSchemes

import ch.ethz.math.ifor.atsp.BranchAndCut.{BranchNode, linearProgrammingSolver}
import ch.ethz.math.ifor.atsp.{Site, arcWise}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

import scala.collection.mutable

object APPricing extends PricingScheme {
  def updateColumns(branchNode: BranchNode): BranchNode = {
    println("start AP pricing")

    System.loadLibrary("jniortools")

    // compute LP reduced costs

    // initialize c_{ij}' = c_{ij}
    var reducedCost = mutable.Map[Site, Map[Site, Double]]()
    for (item <- branchNode.costsMap){
      reducedCost = reducedCost + item
    }
    println("number of constraints: ",branchNode.constraintsInNode)

    for(rowConstraint <- branchNode.constraintsInNode){
      if (rowConstraint.dualValue()!=0.0){
        for(var1 <- branchNode.variables.entries){
          for(var2 <- var1._2){
            if (rowConstraint.getCoefficient(var2._2)!=0 && var1._1!=var2._1){
              //println("coeff before",var1._1,var2._1,reducedCost(var1._1)(var2._1),rowConstraint.getCoefficient(var2._2),rowConstraint.dualValue())
              reducedCost = reducedCost.map{
                case (site1, map1) => (site1, map1.map{
                  case (site2, value) if site1==var1._1 && site2==var2._1 => (site2, value -rowConstraint.dualValue()*rowConstraint.getCoefficient(var2._2))
                  case (site2, value) => (site2, value)
                })
              }

            }
          }
        }
      }
    }

    reducedCost.collect{
      case (site1, map1) => map1.collect{
        case (site2, value) if value ==0 => println(site1, site2, value)
      }
    }
    // calculate cardinality of arcs of negative reduced cost
    var mu :Int = 0
    reducedCost.collect{
      case(site1, map1) => (site1,map1.collect{
        case (site2,value) if value < 0 => mu += 1
      })
    }
    reducedCost.collect{
      case(site1, map1) => (site1,map1.collect{
        case (site2,value) if value < 0 => println(site1,site2)
      })
    }
    println("Number of negative reduced cost arcs: "+mu)

    if (mu==0) return branchNode

    val muThreshold: Int = branchNode.input.sites.length/10

    if(branchNode.isRootNode){
      // take 15 smallest cost arcs leaving each node
      var tmp: Map[Site, List[(Site, Double)]] = Map()

      for (site <- branchNode.input.sites){
        val a = branchNode.costsMap(site).toList.sortWith(_._2 < _._2)
        val b = a.take(15)
        tmp = tmp ++ Map(site -> b)
      }

      branchNode.varAssignment = branchNode.varAssignment.map{
        case (site1, map1) => (site1, map1.map{
          case (site2, value) if tmp(site1).contains((site2, branchNode.costsMap(site1)(site2))) => (site2,value)
          case (site2, value) => (site2,Some(false))
        })
      }
    }

    if (mu <= muThreshold){
      // use standard pricing
      // add arcs of negative cost to core set
      val newAssignment = branchNode.varAssignment.map{
        case (site1, map1) => (site1, map1.map{
          case (site2, value) if reducedCost(site1)(site2) < 0  => (site2, null)
          case (site2, value) => (site2, value)
        })
      }
      branchNode.varAssignment = newAssignment
      return branchNode
    } else{
      // AP pricing
      // solve an AP on reduced cost
      // if min cost of AP solution = 0, break
      // if not, add to core set arcs in the optimal AP solution and iterate
      val solver: MPSolver = new MPSolver("AssignmentProblem",
        MPSolver.OptimizationProblemType.GLOP_LINEAR_PROGRAMMING)

      def constructVariable(site1:Site ,site2:Site):MPVariable=
        if (site1.id==site2.id) {solver.makeNumVar(0,0,"")}
        else {
          //println("branchNode.varAssignment(site1)(site2)",branchNode.varAssignment(site1)(site2))
          branchNode.varAssignment(site1)(site2) match {
            case null => solver.makeNumVar(0,1,"")
            case Some(true)=> solver.makeNumVar(1,1,"")
            case Some(false)=> solver.makeNumVar(0,0,"")
          }
        }

      val x: arcWise[MPVariable] = arcWise(branchNode.input, constructVariable)

      def constructCost(site1:Site, site2:Site):Double={
        reducedCost(site1)(site2)
      }
      val costs:arcWise[Double] = arcWise(branchNode.input,constructCost)

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
      println("AP is: "+resultStatus)

      if (objective.value()==0){
        println("Ap solution value = 0")
        return branchNode
      } else {
        // add arcs in the optimal solution to the core set
        println("Ap solution value is not 0"+ objective.value())
        branchNode.varAssignment.foreach{
          case (site1, map1) => map1.foreach{
            case (site2, value) => println(site1, site2, value)
          }
        }
        println("========================")

        val newAssignment = branchNode.varAssignment.map{
          case (site1, map1) => (site1, map1.map{
            case (site2, value) if x.search(site1,site2).solutionValue == 1.0 => (site2, null)
            case (site2, value) => (site2, value)
          })
        }

        newAssignment.foreach{
          case (site1, map1) => map1.foreach{
            case (site2, value) => println(site1, site2, value)
          }
        }

        val newNode = new BranchNode(branchNode.input,newAssignment,branchNode.globalConstraints,branchNode.formulation)
        // recursion
        updateColumns(newNode)
      }
    }
    }
}
