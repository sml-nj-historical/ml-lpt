(* options.sml
 *
 * COPYRIGHT (c) 2007 Fellowship of SML/NJ
 *
 * Processing of command line arguments
 *)

structure Options = 
  struct

    datatype action_style = ActNormal | ActUnit | ActDebug

    val actStyle : action_style ref	= ref ActNormal
    

  end