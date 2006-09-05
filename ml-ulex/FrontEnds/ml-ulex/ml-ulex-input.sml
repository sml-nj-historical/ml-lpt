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

    structure P = Parser(Streamify)

    fun parseFile fname = let
          fun parseErr (msg, line, _) = 
	        (print (Int.toString line);
		 print ": ";
		 print msg;
		 print "\n")
	  val strm = TextIO.openIn fname
	  val lexer =
		MLULexLex.makeLexer (fn n => TextIO.inputN (strm, n))
	  val (spec, errors) = P.parse (Streamify.streamify lexer)
			       before TextIO.closeIn strm
	  fun errMsg (pos, err) = print (P.repairToString err ^ "\n")
	  in
            app errMsg errors;
	    spec
	  end

  end
