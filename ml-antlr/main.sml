(* main.sml
 *
 * COPYRIGHT (c) 2006 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * The driver.
 *)


structure Main : sig

    val main : (string * string list) -> OS.Process.status

    val load : string -> LLKSpec.grammar * GLA.gla
    val getNT : LLKSpec.grammar -> string -> LLKSpec.nonterm

  end = struct

  (* check a parse tree, returning a grammar *)
    fun checkPT parseTree = let
	  val grm = CheckGrammar.check parseTree
	  val LLKSpec.Grammar {nterms, prods, ...} = grm
	  val _ = Err.debugs [" ", Int.toString (List.length nterms), 
			  " nonterminals"]
	  val _ = Err.debugs [" ", Int.toString (List.length prods), 
			  " productions"]
(*
val _ = app (Err.debug o Prod.toString) prods
*)
          in
            grm
          end

    fun process() = let
	  val _ = Err.anyErrors := false
	  val file = !Options.fname
	  val grm = checkPT (ParseFile.parse file)
	  val gla = GLA.mkGLA grm
	  val pm = ComputePredict.mkPM (grm, gla)
	  val outspec = (grm, pm, file)
	  in
            SMLOutput.output outspec;
            if !Options.dotOutput then GLA.dumpGraph (file, grm, gla) else ();
	    if !Options.texOutput then LaTeXOutput.output outspec else ();
	    if !Err.anyErrors
	      then OS.Process.failure
	      else OS.Process.success
	  end
 	  handle Err.Abort => OS.Process.failure
	  | ex => (
	    Err.errMsg [
		"uncaught exception ", General.exnName ex,
		" [", exnMessage ex, "]"
	      ];
	    List.app (fn s => Err.errMsg ["  raised at ", s]) (SMLofNJ.exnHistory ex);
	    OS.Process.failure)

    fun main (_, args) = 
	  (app Options.procArg args;
	   if String.size (!Options.fname) = 0 then
	     (Err.errMsg ["usage: ml-antlr [--dot] [--latex] file"]; OS.Process.failure)
	   else process())

  (* these functions are for debugging in the interactive loop *)
    fun load file = let
          val grm = checkPT (ParseFile.parse file)
	  val gla = GLA.mkGLA grm
    in
      (grm, gla)
    end

    fun getNT (LLKSpec.Grammar {nterms, ...}) name =
	  hd (List.filter (fn nt => Nonterm.qualName nt = name) nterms)

  end
