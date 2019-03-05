import spatial.dsl._

@spatial object Tiling extends SpatialApp {
  def main(args: Array[String]): Void = {
    // Define data type
    type T = FixPt[TRUE,_24,_8]
    
    // Set vector length
    val all = 64
    val len = 16
    
    // Generate data
    val vec1 = Array.tabulate[T](all){i => i.to[T]}
    val vec2 = Array.tabulate[T](all){i => (all - i).to[T]}
    val d1 = DRAM[T](all)
    val d2 = DRAM[T](all)
    setMem(d1, vec1)
    setMem(d2, vec2)

    // Allocate reg for the answer
    val x = ArgOut[T]
    Accel {
      // Create local SRAMs
      val s1 = SRAM[T](len)
      val s2 = SRAM[T](len)
      
      // Multiply and accumulate
      x := Reduce(Reg[T](0))(all by len){tile => 

        // Transfer data
        s1 load d1( tile::tile + len )
        s2 load d2( tile::tile + len )

        Reduce(Reg[T](0))( len by 1 ){i =>
          s1(i) * s2(i)
        }{_+_}

      }{_+_} 

    }
    
    // Compute "correct" answer
    val gold = vec1.zip(vec2){_*_}.reduce{_+_}
    
    // Check answer
    //assert(gold == getArg(x), r"Expected ${gold}, got ${getArg(x)}!")
    if( gold == getArg(x) ){
      println("Correct")
    }
    else{
      println("WRONG")
    }
    println(r"Expected ${gold}, got ${getArg(x)}!")    
  }
}
