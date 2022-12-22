package cp420

object LinkLoader:
  def load(code: Memory): AbstractMachine =
    /* Put code at 0x100, set pc to 0x100, (why?)
       set sp to 0x00FF_FFFF?, etc. */
    ???

@main def boot =
  /* Run:
     - Load an executable image
     - Find the code stream
     - Find the data page
     - LinkLoader.load( that-stuff )
   */

  println("Hi, mom")