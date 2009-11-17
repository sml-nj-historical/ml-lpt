(* main.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Driver for ml-ulex.
 *)

structure Main = 
  struct

    structure RE = RegExp
    structure Lex = LexGen
    structure LO = LexOutputSpec

    val name = "ml-ulex"
    fun debug s = (print s; print "\n")

  (* count the total number of DFA states *)
    fun numStates (LO.Spec{dfa, ...}) = List.length dfa

    fun status s = debug ("[" ^ name ^ ": " ^ s ^ "]")

    fun mlULex () = let
          val _ = if String.size (!Options.fname) = 0 
		  then (
		    print (concat[
			"No input file specified\n  usage:  ",
			name, " ", Options.args, " file\n"
		      ]);
		    OS.Process.exit OS.Process.failure)
		  else ()
	  val _ = status "parsing"
          val inSpec' = if !Options.lexCompat
			then MLLexInput.parseFile (!Options.fname)
			else MLULexInput.parseFile (!Options.fname)
	  val inSpec = if (!Options.beTest) 
		       then LexSpec.emptyActions inSpec'
		       else inSpec'
	  val _ = status "DFA gen"
	  val outSpec = Lex.gen inSpec
	  val _ = (debug (concat [" ", Int.toString (numStates outSpec),
				  " states in full DFA"]))
	  val _ = if !Options.dump then
		    (status "DFA dump";
		     DumpOutput.output (outSpec, !Options.fname))
		  else ()
	  val _ = if !Options.dot then
		    (status "DOT gen";
		     DotOutput.output (outSpec, !Options.fname))
		  else ()
	  val _ = status "SML gen"
	  val _ = if (numStates outSpec > 150 andalso !Options.beMode = Options.BySize)
		     orelse !Options.beMode = Options.TableBased
		  then SMLTblOutput.output (outSpec, !Options.fname) 
		  else SMLFunOutput.output (outSpec, !Options.fname)
	  val _ = if !Options.match then 
		    (debug "-- Interactive matching (blank line to quit) --";
		     Match.output (outSpec, !Options.fname))
		  else ()
	  in
            OS.Process.success
          end

    fun main (_, args) = let
	  val _ = List.app Options.procArg args
	  in 
	    mlULex()
          end
	    handle ex => (
	      TextIO.output(TextIO.stdErr, concat[
		  "uncaught exception ", General.exnName ex,
		  " [", General.exnMessage ex, "]\n"
	        ]);
	      app (fn s => TextIO.output(TextIO.stdErr, concat[
		  "  raised at ", s, "\n"
	        ]))
	        (SMLofNJ.exnHistory ex);
	      OS.Process.exit OS.Process.failure)

  end
