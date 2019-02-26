import spatial.dsl._

@spatial object Add extends SpatialApp {
  def main(args: Array[String]): Void = {
    // Create Args
    val x = ArgIn[Int]
    val y = ArgIn[Int]
    val z = ArgOut[Int]
    
    // Set `x` to the value of the first command line argument
    setArg(x, args(0).to[Int])
    // Set `z` to the value of the second command line argument
    setArg(y, args(1).to[Int])
    
    Accel {
      // Set `z` to the sum of `x` and `y`
      z := x + y
    }
    // Report the answer
    println(r"Result is ${getArg(z)}")
  }
}
