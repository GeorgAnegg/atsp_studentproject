package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers

import ch.ethz.math.ifor.atsp.{Site, arcWise}
import com.google.ortools.linearsolver.MPVariable

case class arcVariables(entries: Map[Site, Map[Site, MPVariable]]) extends arcWise[MPVariable](entries)
