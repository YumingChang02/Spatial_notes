import spatial.dsl._

@spatial object Conv2D extends SpatialApp {

  def main(args: Array[String]): Unit = {
    type T = FixPt[ TRUE, _16, _16 ]

    // parameters
    val input_h   = 64
    val input_w   = 64
    val tile_size = 16
    val kernel_h = 3
    val kernel_w = 3

    // variables genetrated from parameters
    val kernel_size = kernel_h * kernel_w
    val extended_tile = tile_size + kernel_w - 1

    // load data
    val input  = ( 0::input_h, 0::input_w  ){ ( i, j ) =>
        ( i * input_w + j ).to[T]
    }
    
    // need to reverse kernel
    val kernel = ( 0::kernel_h, 0::kernel_w ){ ( i, j ) =>
        ( kernel_size / 2 - i * kernel_w - j ).to[T]
    }
    
    // set destination to all zero
    val output  = ( 0::( input_h + kernel_h - 1 ), 0::( input_w + kernel_w - 1 ) ){
        (_,_) => 0.to[T] // since we don't need i and j values for initing zeros
    }

    // Create input and output DRAMs
    val srcmem = DRAM[T](  input_h, input_w  )
    val kermem = DRAM[T]( kernel_h, kernel_w )
    val dstmem = DRAM[T]( input_h + kernel_h - 1, input_w + kernel_w - 1 )  // latter change to consider stride / padding

    // copy variable to memory
    setMem( srcmem, input  )
    setMem( kermem, kernel )
    setMem( dstmem, output )

    Accel{
        // input buffer from DRAM
        val buffer = LineBuffer[T]( kernel_h, tile_size )
        
        // shift registers for decouple
        val shtreg = RegFile[T]( kernel_h, kernel_w )
        
        // sram for input tiling block
        val kernel_sram = SRAM[T]( kernel_h, kernel_w  )
        val output_sram = SRAM[T]( extended_tile ) // latter change to consider stride / padding
        val temp_output = SRAM[T]( extended_tile )
        
        // load kernel
        kernel_sram load kermem

        // slice input_width by tile size for better reuse
        Foreach( input_w by tile_size ){ iw =>  // input width by tile size
            
            // tiling load
            Foreach( input_h by 1 ){ ih =>  // input height
            
                // load new tile into buffer
                buffer load srcmem( ih, iw::iw + tile_size )
            
                // if this is the starting of the line, reset
                Pipe{ shtreg.reset( true ) }
                
                // load last output for overlapping
                temp_output load dstmem( ih, iw::iw + extended_tile )

                // shift for convoluton
                Foreach( extended_tile by 1 ){ iw_t => // output index of tile
                    
                    // shift input into shift registers
                    Parallel{
                        Foreach( kernel_h by 1 ) { kh =>
                            shtreg( kh, * ) <<= mux( iw_t < tile_size, buffer( kh, iw_t ), 0 )
                        }
                    }
                    
                    output_sram( iw_t ) = Reduce( Reg[T] ( 0.to[T] ) )( kernel_h by 1, kernel_w by 1 ){ ( j, k ) =>
                        shtreg( j, k ) * kernel_sram( j, k )
                    }{ _ + _ } + temp_output( iw_t )
                    
                }
                
                //store results
                dstmem( ih, iw::iw + extended_tile ) store output_sram

            }
        }
    }

    // Extract results from accelerator
    val results = getMatrix( dstmem )

    // Create validation checks and debug code
    // printMatrix( input, "input:" )
    printMatrix( kernel, "kernel:" )
    printMatrix( results, "Results:" )
  }

}
