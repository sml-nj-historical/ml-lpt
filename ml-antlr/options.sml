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
    val dotOutput : bool ref		= ref false
    val texOutput : bool ref		= ref false
    val fname : string ref		= ref ""

    fun procArg arg = 
	  (case arg
	    of "--dot"    => dotOutput := true
	     | "--latex"  => texOutput := true
	     | "--unit-actions" => actStyle := ActUnit
	     | file	  => 
	         if String.size (!fname) > 0 
		 then 
		   raise Fail "Only one input file may be specified\n"
		 else fname := file
	   (* end case *))

  end