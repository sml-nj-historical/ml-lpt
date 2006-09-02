structure Main = 
  struct

    structure P = Parser(Streamify)

    fun parse strm = let
	  val lexer =
		Mlex.makeLexer (fn n => TextIO.inputN (strm, n))
	  val (p, errors) = P.parse (Streamify.streamify lexer)
			       before TextIO.closeIn strm
	  fun errMsg (pos, err) = print (P.repairToString err ^ "\n")
	  in
            app errMsg errors;
	    p
	  end

    fun errMsg l = 
	  TextIO.output(TextIO.stdErr, String.concat l)

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
