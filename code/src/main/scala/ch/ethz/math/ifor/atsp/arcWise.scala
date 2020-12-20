package ch.ethz.math.ifor.atsp
import ch.ethz.math.ifor.atsp.{Site}

class arcWise[T] (val entries: Map[Site, Map[Site, T]]) {

  def search(site1: Site,site2:Site):T = {
    entries(site1)(site2)
  }

/*
  def inDegreeSites(site:Site):List[Site]={
    var result: List[(Site,T)] = List()

    entries.foreach{
      case (site1, map1) => (site1,map1.foreach{
        case (site2, value) if site2 == site => result = result ::: (site1,value) :: Nil
      }
      )
    }

    val sorted_list = result.sortBy(_._2)()

  }

 */
/*
  def subtractInDegree(site:Site,cost:T):Map[Site, Map[Site, T]]={
    entries.collect{
      case (site1, map1) => (site1,map1.collect{
        case (site2, value) if site2 == site => (site2, value-cost)
        case (site2, value) if site2 != site => (site2, value)
      })
    }
  }


 */


}

object arcWise {

  def apply[T](input: Input,function: (Site,Site)=> T): arcWise[T] = new arcWise(constructEntries(input, function))
  
  def constructEntries[T](input:Input, function: (Site,Site)=> T): Map[Site, Map[Site, T]] =
  input.sites.map(  site1=> (site1, input.sites.map(   site2 => (site2,function(site1,site2)) ).toMap)   ).toMap

}