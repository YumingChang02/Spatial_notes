import spatial.dsl._

@spatial object Conv extends SpatialApp {

  def main(args: Array[String]): Unit = {
    type T = FixPt[ TRUE, _16, _16 ]

    // parameters
    val input_size  = 16 // current same for sliding window and tile size, may change
    val kernel_size = 3

    // load data
    val input  = Array.tabulate[T]( input_size  ){ i => i.to[T] }
    val kernel = Array.tabulate[T]( kernel_size ){ i => ( kernel_size - i - 1 ).to[T] }

    // Create input and output DRAMs
    val srcmem = DRAM[T]( input_size )
    val dstmem = DRAM[T]( input_size )
    val kermem = DRAM[T]( kernel_size )

    // copy variable to memory
    setMem( srcmem, input  )
    setMem( kermem, kernel )

    Accel{
        // sliding window
        val sldwin = RegFile[T]( kernel_size )

        // allocate memory for input output and kernel
        val rawdata = SRAM[T]( input_size ) // tile size
        val result  = SRAM[T]( input_size ) // tile size
        val kersram = SRAM[T]( kernel_size )

        // load tile memory
        rawdata load srcmem // need to add offset if tiling
        kersram load kermem

        Foreach( input_size by 1 ){ j =>

            // Shift next element into sliding window
            sldwin <<= rawdata( j )

            // compute convolution
            result(j) = Reduce( Reg[T] ( 0.to[T] ) )( kernel_size by 1 ){ i=>
                sldwin( i ) * kersram( i )
            }{ _ + _ }

        }

        // store result
        dstmem store result

    }

    // Extract results from accelerator
    val results = getMem( dstmem )

    // Create validation checks and debug code
    printArray( results, "Results:" )

  }

}
