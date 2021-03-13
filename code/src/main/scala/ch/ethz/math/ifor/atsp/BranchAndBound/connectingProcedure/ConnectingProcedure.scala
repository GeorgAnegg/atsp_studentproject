package ch.ethz.math.ifor.atsp.BranchAndBound.connectingProcedure
import ch.ethz.math.ifor.atsp.{Site}
import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
object ConnectingProcedure {

  def reduceNumberOfSubtours(branchNode: BranchNode): (Map[Site,Map[Site,Boolean]],Boolean,Boolean) = {

    var resultAssignment : Map[Site,Map[Site,Boolean]] = branchNode.lowerBoundSolve

    val zeroArcsThreshold : Double = branchNode.input.sites.length * 2.5

    val numberOfZeros:Double={
      var count = 0
      branchNode.reducedCostMatrixAfterAP.collect{
        case (_,map1) => map1.collect{ case (_,value) if value == 0 => count += 1}
      }
      count
    }

    if(numberOfZeros > zeroArcsThreshold || branchNode.isRootNode) {
      println("need connecting")
      var tours: List[Map[Site, Site]] = List()
      branchNode.allTours.foreach(tour => tours = tours ::: tour.listArcs :: Nil)
      //println("size of subtours before connecting: ",tours.size)

      def patchTwoTours(arc1:Map[Site,Site],arc2:Map[Site,Site],tour1:Map[Site,Site],tour2:Map[Site,Site]): Map[Site,Site] ={
        var result :Map[Site,Site]= tour1 ++ tour2
        result = result - arc1.head._1
        result = result - arc2.head._1
        result = result ++ Map(arc1.head._1->arc2.head._2)
        result = result ++ Map(arc2.head._1->arc1.head._2)
        result
      }

      var toursMap: Map[Map[Site, Site], Boolean] = Map()
      tours.foreach(t => toursMap = toursMap ++ Map(t -> false))

      while (tours.size>1 && toursMap.values.exists(_==false)){
        /*
        println("tour size: ", tours.size)
        for (i<-toursMap){
          i._1.foreach{e=>println(e._1,e._2,i._2)}
          println("=====\r\n")
        }

         */
        var currentTour = tours.head
        //println("current tour")
        currentTour.foreach{e => println(e._1,e._2)}
        tours = tours.filter(_!=currentTour)
        toursMap = toursMap.updated(currentTour,true)
        var alreadyPatched = false
        /*
        for (otherTour<-tours){
          for (arcCurrent <- currentTour){
            for (arcOther <- otherTour){
              if (branchNode.reducedCostMatrixAfterAP(arcCurrent._1)(arcOther._2)==0 &&
                branchNode.reducedCostMatrixAfterAP(arcCurrent._2)(arcOther._1)==0){
                // patch
                currentTour = patchTwoTours(Map(arcCurrent),Map(arcOther),currentTour,otherTour)
                tours = tours.filter(_!=otherTour)
                alreadyPatched = true
              }
            }
          }
        }
         */
        var localOthers = tours
        while (localOthers.nonEmpty && !alreadyPatched){
          var tourA = localOthers.head
          //println("tourA",tourA.size)
          //tourA.foreach{e => println(e._1,e._2)}
          var localCurrentTour = currentTour
          while (localCurrentTour.nonEmpty && !alreadyPatched){
            var arcCurrent = localCurrentTour.head
            //println("current arc ",arcCurrent._1,arcCurrent._2,"current size ",localCurrentTour.size)
            var count = 0
            while (count!=tourA.size && !alreadyPatched){
              var arcOther = tourA.head
              count += 1
              //println("tour A arc ",arcOther._1,arcOther._2)
              if (branchNode.reducedCostMatrixAfterAP(arcCurrent._1)(arcOther._2)==0 &&
                branchNode.reducedCostMatrixAfterAP(arcCurrent._2)(arcOther._1)==0){
                // patch
                /*
                println("patching")

                println("patch ",arcCurrent._1,arcCurrent._2,arcOther._1,arcOther._2)
                println("current")
                currentTour.foreach{e => println(e._1,e._2)}
                println("other")
                tourA.foreach{e => println(e._1,e._2)}

                println("")
                println("")
                //currentTour.foreach{e => println()}
                println("")
                //tourA.foreach{e => println()}

                 */

                currentTour = patchTwoTours(Map(arcCurrent),Map(arcOther),currentTour,tourA)
                tours = tours.filter(_!=tourA)
                toursMap = toursMap.updated(tourA,true)
                tours = tours :+ currentTour
                toursMap = toursMap ++ Map(currentTour->false)
                alreadyPatched = true

                //println("=====new tour======")
                //currentTour.foreach{e => println(e._1,e._2)}

                resultAssignment = resultAssignment.map{
                  case (site1, map1) => (site1, map1.map{
                    case (site2, value) if ((site1==arcCurrent._1 && site2==arcCurrent._2) ||
                      (site1==arcOther._1 && site2==arcOther._2)) => (site2,false)
                    case (site2, value) if ((site1==arcCurrent._1 && site2==arcOther._2) ||
                      (site1==arcOther._1 && site2==arcCurrent._2)) => (site2,true)
                    case (site2,value) => (site2, value)
                  })
                }
              } else {
                //println("no patch for",arcCurrent._1,arcCurrent._2,arcOther._1,arcOther._2)
              }
            }
            localCurrentTour = localCurrentTour - arcCurrent._1
            tourA = localOthers.head
          }
          localOthers = localOthers.filter(_!=tourA)
        }
        if(!alreadyPatched){
          tours = tours :+ currentTour
        }
      }
      if (tours.size==1){
        // return optimal tour
        //println("size of subtours after connecting: ",tours.size)
        //tours.head.foreach{e=>println(e._1,e._2)}
        return (resultAssignment,true,true)
      } else {
        //return alternative tour
        //println("size of subtours after connecting: ",tours.size,toursMap.values.exists(_==false))
        //println("toursMap")
        /*
        for (i<-toursMap){
          i._1.foreach{e=>println(e._1,e._2,i._2)}
          println("=====\r\n")
        }
        println("tours")
        for (i<-tours){
          i.foreach{e=>println(e._1,e._2)}
          println("=====\r\n")
        }
        resultAssignment.map{
          case (site1, map1) => (site1, map1.map{
            case (site2,value) => println(site1,site2,value)
          })
        }

         */
        return (resultAssignment,true,false)
      }
    }
    println("do not need connecting")
    (branchNode.lowerBoundSolve, false, false)
    }
}
