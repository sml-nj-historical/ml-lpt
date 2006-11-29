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

    structure L = MLULexLex
    structure P = Parser(L)

    fun parseFile fname = let
          fun parseErr (msg, line, _) = 
	        (print (Int.toString line);
		 print ": ";
		 print msg;
		 print "\n")
	  val fstrm = TextIO.openIn fname
	  val strm = L.streamify (fn n => TextIO.inputN (fstrm, n))
	  val sm = L.mkSourcemap()
	  val lex = L.lex sm
	  val (spec, strm', errors, anns) = 
	        P.parse lex strm
		before TextIO.closeIn fstrm
	  fun errMsg ty (pos, err) = print (String.concat [
		" ", fname, ":",
		     Int.toString (L.getLineNo sm pos), ".",
		     Int.toString (L.getColNo sm pos), 
		ty, err, "\n"])
	  in
            app (errMsg " Syntax error: ") 
	        (map (fn (p, e) => (p, P.repairToString e)) errors);
	    app (errMsg " ") 
		(map (fn ((p, _), e) => (p, e)) anns);
	    spec
	  end

  end
