package ch.ethz.math.ifor.atsp.CompactFormulations

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.{Input, Site}

trait CompactFormulation {
  def solve(input: Input): Map[Site,Map[Site,Boolean]]
}
