package ch.ethz.math.ifor.atsp

class arcWise[T] (val entries: Map[Site, Map[Site, T]]) {

  def search(site1: Site,site2:Site):T = {
    entries(site1)(site2)
  }
}

object arcWise {

  def apply[T](input: Input,function: (Site,Site)=> T): arcWise[T] = new arcWise(constructEntries(input, function))
  
  def constructEntries[T](input:Input, function: (Site,Site)=> T): Map[Site, Map[Site, T]] =
  input.sites.map(  site1=> (site1, input.sites.map(   site2 => (site2,function(site1,site2)) ).toMap)   ).toMap

}