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

    structure P = Parser(MLULexLex)

    fun parseFile fname = let
          fun parseErr (msg, line, _) = 
	        (print (Int.toString line);
		 print ": ";
		 print msg;
		 print "\n")
	  val fstrm = TextIO.openIn fname
	  val strm = MLULexLex.streamify (fn n => TextIO.inputN (fstrm, n))
	  val (spec, _, errors) = P.parse strm
			       before TextIO.closeIn fstrm
	  fun errMsg (pos, err) = print (String.concat [
		"[", Int.toString (MLULexLex.getLineNo pos + 1), "]: ",
		P.repairToString err, 
		"\n"])
	  in
            app errMsg errors;
	    spec
	  end

  end
