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

  (* glue together the lexer and parser *)
    structure LLKLrVals = MLYLrValsFun (structure Token = LrParser.Token)
    structure LLKLex = MLYLexFn (structure Tok = LLKLrVals.Tokens)
    structure LLKParser = JoinWithArg(
      structure ParserData = LLKLrVals.ParserData
      structure Lex = LLKLex
      structure LrParser = LrParser)

    fun debug s = (print s; print "\n")
    fun debugs ss = debug (concat ss)

  (* parse a file, returning a parse tree *)
    fun parseFile filename = let
	  val _ = debug "[ml-antlr: parsing]"
	  val file = TextIO.openIn filename
	  fun get n = TextIO.inputN (file, n)
	  val lexer = LLKParser.makeLexer get (Err.lexErr filename)
	  in
	    #1(LLKParser.parse(15, lexer, Err.parseErr filename, Err.parseErr filename))
	      before TextIO.closeIn file
	  end

  (* check a parse tree, returning a grammar *)
    fun checkPT parseTree = let
	  val _ = debug "[ml-antlr: checking grammar]"
	  val grm = CheckGrammar.check parseTree
	  val LLKSpec.Grammar {nterms, prods, ...} = grm
	  val _ = debugs [" ", Int.toString (List.length nterms), 
			  " nonterminals"]
	  val _ = debugs [" ", Int.toString (List.length prods), 
			  " productions"]
(* val _ = app (debug o Prod.toString) prods *)
          in
            grm
          end

    fun process file = let
	  val _ = Err.anyErrors := false;
	  val grm = checkPT (parseFile file)
	  val gla = GLA.mkGLA grm
	  val pm = ComputePredict.mkPM (grm, gla)
	  val outspec = (grm, pm, file)
	  in
            GLA.dumpGraph (grm, gla);
            SMLOutput.output outspec;
	    LaTeXOutput.output outspec;
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

    fun main (_, [file]) = process file
      | main _ = (print "usage: ml-antlr grammarfile\n"; OS.Process.failure)

  (* these functions are for debugging in the interactive loop *)
    fun load file = let
          val grm = checkPT (parseFile file)
	  val gla = GLA.mkGLA grm
    in
      (grm, gla)
    end

    fun getNT (LLKSpec.Grammar {nterms, ...}) name =
	  hd (List.filter (fn nt => Nonterm.qualName nt = name) nterms)

  end
