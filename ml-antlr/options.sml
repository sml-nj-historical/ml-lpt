(* options.sml
 *
 * COPYRIGHT (c) 2007-2016 Fellowship of SML/NJ
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

  (* process the command line arguments; return true if there is an error *)
    fun processArgs args = let
	  fun procArg "--dot" = (dotOutput := true; false)
	    | procArg "--latex" = (texOutput := true; false)
	    | procArg "--unit-actions" = (actStyle := ActUnit; false)
	    | procArg "--debug" = (actStyle := ActDebug; false)
	    | procArg _ = true
	  in
	    case List.filter procArg args
	     of [file] => (fname := file; false)
	      | _ => true (* error: exactly one file should be specified *)
	    (* end case *)
	  end

  (* usage message *)
    val usage = "usage: ml-antlr [--dot] [--latex] [--unit-actions | --debug] <file>"

  end