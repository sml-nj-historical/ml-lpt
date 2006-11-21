structure Main = 
  struct

    structure L = Mlex
    structure P = Parser(L)

    fun errMsg l = 
	  TextIO.output(TextIO.stdErr, String.concat l)

    fun parse strm = let
	  val s =
		Mlex.streamify (fn n => TextIO.inputN (strm, n))
	  val (p, s', errors, _) = P.parse s before TextIO.closeIn strm
	  fun doErr (pos, err) = errMsg [" ", Int.toString (L.getLineNo (s', pos)), ".",
					 Int.toString (L.getColNo  (s', pos)), " ",
					 "syntax error: ", P.repairToString err, "\n"]
	  in
            app doErr errors;
	    p
	  end

    fun main (_, [file]) = (
	  print ("\n -- ECHO -- \n\n" ^ (parse (TextIO.openIn file)));
	  print "\n\n";
	  OS.Process.success)
	  handle ex => (
	    errMsg [
		"uncaught exception ", General.exnName ex,
		" [", exnMessage ex, "]\n"
	      ];
	    List.app (fn s => errMsg ["  raised at ", s, "\n"]) (SMLofNJ.exnHistory ex);
	    OS.Process.failure)

  end
