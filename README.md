StopWatch
=========

This is a simple demo of [CLaSH](http://www.clash-lang.org/)
tool for Haskell compilation to `.vhdl`.

Uses one algebraic datatype, and two types of counters as Mealy machines.

Displays seconds since reset on a seven segment display.

Recompilation
=============
Not all steps are automated yet, so you may need to do few steps to
get it on your Papillio:

0. For simulation just compile it with `clash` as a Haskell program, and run.

1. For VHDL generation:
    * run interpreter: `clash --interactive DigitalClock.hs`
    * enter `:vhdl` in the interpreter to generate `.vhdl` code
   
2. Refresh _Xilinx ISE_ project by:
    * removing all previously generated `.vhdl` files:
       `rm -rf xst/ DigitalClock/*.vhdl`
    * clicking on ISE - it will ask you to remove files from the project, mark checkbox _yes_.
    * copy new VHDL files: `cp -r vhdl/DigitalClock/*.vhdl DigitalClock/`
    * re-add these files as source files in Xilinx ISE (right click within project window)
    
3. Recompile _Xilinx ISE_ project and generate new bitcode file `SevenSeg.bit`.
    * If there are any errors, compare type of `recordish` in `topEntity.vhdl` wrapper file,
    and generated `DigitalClock/topEntity_0.vhdl` file. Modify the name of the datatype accordingly in two selectors just below.
    * Make sure the toolchain follows all the steps until generation of code for the target device.
        (Note that for a different device you may just provide different `constraints.ucf` file to ISE,
        since this file maps VHDL signal names to FPGA pins.)
        
4. Upload the bitcode onto your FPGA device: `papillio-prog -f SevenSeg.bit`
