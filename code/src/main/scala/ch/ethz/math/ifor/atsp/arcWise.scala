package ch.ethz.math.ifor.atsp

abstract class arcWise[T] (entries: Map[Site, Map[Site, T]]) {


}

object arcWise {
  def construct[T](input:Input, function: (Site,Site)=> T):arcWise[T] =
    arcWise[T](
      input.sites.map(site1=> site1 -> input.sites.map(site2=> site2 -> function(site1,site2)).toMap).toMap
    )

}