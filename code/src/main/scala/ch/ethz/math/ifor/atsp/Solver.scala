package ch.ethz.math.ifor.atsp

trait Solver {

  def solve (input: Input,formulation:String, preprocessing:Boolean,useAddditive:Boolean): Output

}
