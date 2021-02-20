package ch.ethz.math.ifor.atsp.BranchAndCut.pricingSchemes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.Site
import com.google.ortools.linearsolver.{MPObjective, MPVariable}

import scala.collection.mutable

object APPricing extends PricingScheme {
  def updateColumns(branchNode: BranchNode): Map[Site, Map[Site, Double]] = {

    // compute LP reduced costs

    // initialize c_{ij}' = c_{ij}
    var reducedCost = mutable.Map[Site, Map[Site, Double]]()
    for (item <- branchNode.costsMap){
      reducedCost = reducedCost + item
    }

    // update c_{ij}' row by row
    for(rowConstraint <- branchNode.constraintsInNode){
      if (rowConstraint.dualValue()!=0.0){
        for(var1 <- branchNode.variables.entries){
          for(var2 <- var1._2){
            if (rowConstraint.getCoefficient(var2._2)!=0){
              reducedCost(var1._1).updated(var2._1,reducedCost(var1._1)(var2._1)-
                rowConstraint.dualValue()*rowConstraint.getCoefficient(var2._2))
            }
          }
        }
      }
    }

    // calculate cardinality of arcs of negative reduced cost
    var mu :Int = 0
    reducedCost.collect{
      case(site1, map1) => (site1,map1.collect{
        case (site2,value) if value < 0 => mu += 1
      })
    }

    val muThreshold:Int = branchNode.input.sites.length/10

    if (mu==0) return branchNode.lowerBoundSolve

    if (mu <= muThreshold){
      // add arcs of negative cost to core set
      reducedCost.foreach{
        case(site1, map1) => (site1,map1.foreach{
          case (site2,value) if value < 0 => branchNode.varAssignment(site1).updated(site2,null)
        })
      }
      updateColumns(branchNode)
    } else{
      var newObjective: MPObjective = branchNode.solverLP.objective()
      branchNode.variables.entries.map{
        case (site1, map1) => (site1, map1.map{
          case (site2, variable) => newObjective.setCoefficient(variable,reducedCost(site1)(site2))
        })
      }
      newObjective.setMinimization()
      val result = branchNode.solverLP.solve()
      while(newObjective.value()!=0){
        updateColumns(branchNode)
      }
      branchNode.lowerBoundSolve
    }

    }

}
