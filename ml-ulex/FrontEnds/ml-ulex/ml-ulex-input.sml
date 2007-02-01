(* ml-ulex-input.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Driver for ml-ulex input format.
 *)

structure MLULexInput =
  struct

    structure SP = StreamPos
    structure L = MLULexLex
    structure P = MLULexParseFn(L)

    fun parseFile fname = let
          fun parseErr (msg, line, _) = 
	        (print (Int.toString line);
		 print ": ";
		 print msg;
		 print "\n")
	  val fstrm = TextIO.openIn fname
	  val strm = L.streamifyInstream fstrm
	  val sm = SP.mkSourcemap()
	  val lex = L.lex sm
	  val (spec, strm', errors, {errs}) = 
	        P.parse lex strm
		before TextIO.closeIn fstrm
	  fun errMsg ty (pos, err) = print (String.concat [
		" ", fname, ":",
		     Int.toString (SP.lineNo sm pos), ".",
		     Int.toString (SP.colNo sm pos), 
		ty, err, "\n"])
	  in
            app (errMsg " Syntax error: ") 
	        (map (fn (p, e) => 
			 (p, Repair.actionToString MLULexTokens.toString e)) 
		     errors);
	    app (errMsg " ") 
		(map (fn ((p, _), e) => (p, e)) errs);
	    case spec
	     of SOME s => s
	      | NONE => OS.Process.exit OS.Process.failure
	  end

  end
