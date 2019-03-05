import spatial.dsl._

@spatial object Conv extends SpatialApp {

  def main(args: Array[String]): Unit = {
    type T = FixPt[ TRUE, _16, _16 ]

	// parameters
	//val input_size  = ArgIn[Int]
    val tiling_size = 16 // current same for sliding window and tile size, may change
	val kernel_size = 5

	// load data
    //setArg( input_size, args(0).to[Int] )
	val input  = Array.tabulate[T]( tiling_size ){ i => i.to[T] } // TODO change to input size
	val kernel = Array.tabulate[T]( kernel_size ){ i => i.to[T] }

	// Create input and output DRAMs
	val srcmem = DRAM[T]( tiling_size ) // TODO latter change to input size
	val dstmem = DRAM[T]( tiling_size ) // TODO latter change to input size
	val kermem = DRAM[T]( kernel_size )

	// copy variable to memory
	setMem( srcmem, input  )
	setMem( kermem, kernel )

	Accel{
		// sliding window
		val sldwin = RegFile[T]( kernel_size )

		// allocate memory for input output and kernel
		val rawdata = SRAM[T]( tiling_size ) // tile size
		val result  = SRAM[T]( tiling_size ) // tile size
		val kersram = SRAM[T]( kernel_size )

		// load tile memory
		rawdata load srcmem // need to add offset if tiling
		kersram load kermem( kernel_size::0 ) //trying reverse the kernel here ( <<= pushes the value to HEAD )

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
