package ch.ethz.math.ifor.atsp

case class distanceMatrix(entries: Map[Site, Map[Site, Double]]) extends arcWise[Double](entries)
