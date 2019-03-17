import spatial.dsl._

@spatial object Conv_tile extends SpatialApp {

  def main(args: Array[String]): Unit = {
    type T = FixPt[ TRUE, _16, _16 ]

    // parameters
    val input_size = 1024
    val tile_size  = 64 // current same for sliding window and tile size, may change
    val kernel_size = 9

    // load data
    val input  = Array.tabulate[T]( input_size  ){ i => i.to[T] }
    val kernel = Array.tabulate[T]( kernel_size ){ i => ( kernel_size / 2 - i ).to[T] }

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
        val rawdata = SRAM[T]( tile_size ) // tile size
        val result  = SRAM[T]( tile_size ) // tile size
        val kersram = SRAM[T]( kernel_size )

        // load tile memory
        kersram load kermem

        Foreach( input_size by tile_size ){ i =>

            rawdata load srcmem( i::i+tile_size )

            Foreach( tile_size by 1 ){ j =>

                // Shift next element into sliding window
                sldwin <<= rawdata( j )

                // compute convolution
                result(j) = Reduce( Reg[T] ( 0.to[T] ) )( kernel_size by 1 ){ i=>
                    sldwin( i ) * kersram( i )
                }{ _ + _ }

            }

            // store result
            dstmem( i::i+tile_size ) store result

        }

    }

    // Extract results from accelerator
    val results = getMem( dstmem )

    // Create validation checks and debug code
    printArray( input, "input:" )
    printArray( kernel, "kernel:" )
    printArray( results, "Results:" )

  }

}
