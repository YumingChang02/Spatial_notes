import spatial.dsl._

@spatial object Fibonacci extends SpatialApp {
  def main(args: Array[String]): Void = {
    // Create DRAM (malloc)
    val d = DRAM[Int](16)
    
    // Set DRAM (memcpy)
    val data = Array.fill[Int](16)(0)
    setMem(d, data)
    
    Accel {
      // Create 16-element SRAM
      val s = SRAM[Int](16)
      
      // Transfer data from d to s
      s load d(0::16)
    
      // Add number to each element
      s(0) = 1;
      s(1) = 1;
      Foreach(14 by 1){i => s(i+2) = s(i+1) + s(i)}

      // Transfer data back to d
      d(0::16) store s
    }
    
    // Print contents in memory
    printArray(getMem(d), "Result: ")

  }
}
