package ch.ethz.math.ifor.atsp.CompactFormulations

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.{Input, Output, Site}

trait CompactFormulation {
  def solve(input: Input): (Map[Site,Map[Site,Boolean]],Output)
}
