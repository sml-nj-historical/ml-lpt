(* parse-file.sml
 *
 * COPYRIGHT (c) 2006 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Driver for the parser.
 *)

structure ParseFile = 
  struct

    structure P = SpecParseFn(SpecLex)

    fun parse filename = let
	  val _ = Err.status ("parsing " ^ filename) 
	  val file = TextIO.openIn filename
          val sm = StreamPos.mkSourcemap()
	  val strm = SpecLex.streamifyInstream file
	  val (res, strm', errs) = P.parse (SpecLex.lex sm) strm
	  fun err2str err = Repair.repairToString SpecTokens.toString sm err ^ "\n"
          in 
            app (print o err2str) errs;
            TextIO.closeIn file;
            res  
          end

  end