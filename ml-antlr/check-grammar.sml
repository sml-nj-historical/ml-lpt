(* check-grammar.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Check a parse tree, returning a grammar.
 *)

structure CheckGrammar : sig

    val check : GrammarSyntax.grammar -> LLKSpec.grammar

  end = struct

    structure Syn = GrammarSyntax
    structure ATbl = AtomTable

    fun nextId (r : int ref) () = let val id = !r in r := id+1; id end

  (* count the number of rules; we start the subrule IDs off at this value
   * to keep the two distinct.
   *)
    fun countRules rules = let
	  fun cnt (Syn.RULE{alts, ...}, n) = n + List.length alts
	  in
	    List.foldl cnt 0 rules
	  end

    fun loadToks (nextGlobalID, toks) = let
	  val tokTbl = ATbl.mkTable (64, Fail "token table")
	  val tokList = ref []
	  fun addTok (name, ty, abbrev) = (case ATbl.find tokTbl name
		 of NONE => let
		      val id = nextGlobalID()
		      val info = LLKSpec.T{name = name, id = id, ty = ty, abbrev = abbrev}
		      in
			ATbl.insert tokTbl (name, info); 
			Option.app (fn a => ATbl.insert tokTbl (a, info)) abbrev;
			tokList := info :: !tokList
		      end
		  | SOME info => raise Fail ("Token '" ^ 
					     Atom.toString name ^ 
					     "' defined multiple times")
		(* end case *))
          in
            List.app addTok toks;
	    (List.rev (!tokList), tokTbl)
          end

    fun check (g : Syn.grammar) = let
	  val nextGlobalID = nextId (ref 0)
	  val Syn.GRAMMAR {header, defs, rules, toks, actionStyle} = g
          val (tokList, tokTbl) = 
	      loadToks (nextGlobalID, (Atom.atom "EOF", NONE, NONE)::toks)
	  fun lookupTok name = (case ATbl.find tokTbl name
		 of NONE => raise Fail ("Token '" ^
					Atom.toString name ^
					"' is undefined")
		  | SOME info => info
		(* end case *))
	(* keeping track of nonterminals *)
	  val numNTerms = ref 0
	  val ntTbl = ATbl.mkTable (64, Fail "nonterm table")
	  val ntList = ref []
	  fun insNTerm (nt as LLKSpec.NT{name, ...}) =
	        (ATbl.insert ntTbl (name, nt);
		 ntList := nt :: !ntList;
		 nt)
	(* map a non-terminal name to its info record *)
	  fun lookupNTerm name = (case ATbl.find ntTbl name
		 of NONE => insNTerm (LLKSpec.NT{name = name, id = nextGlobalID(), formals = ref [],
						 binding = LLKSpec.TOP, prods = ref[], isEBNF = false}) 
		  | SOME info => info
		(* end case *))
	(* keeping track of productions *)
	  val prodList = ref []
	(* check a nonterminal *)
          fun loadNTerm(nt, newFormals, alts) = let
(* val _ = print(concat["chkRule: ", Atom.toString lhs, "\n"]); *)
		val LLKSpec.NT{prods, formals, ...} = nt
		val nextProdID = nextId (ref 1)
		fun doAlt (rhs) = let
		      val Syn.ALT {items, action, try, pred} = rhs
		      val nextSRID = nextId (ref 1)
		      fun doItem (Syn.SYMBOL (name, args)) = 
			    if ATbl.inDomain tokTbl name
			    then if not (isSome args)
			         then LLKSpec.TOK(lookupTok name)
			         else raise Fail 
				   ("Attempted to apply arguments to token '" 
				    ^ (Atom.toString name)
				    ^ "'")
			    else LLKSpec.NONTERM(lookupNTerm name, 
						 Option.map Action.action args)
			| doItem (Syn.SUBRULE alts) =
			    LLKSpec.NONTERM(doSubrule (false, alts), NONE)
			| doItem (Syn.CLOS itm) =
			    LLKSpec.CLOS(doSubrule (true, mkAlts itm))
			| doItem (Syn.POSCLOS itm) =
			    LLKSpec.POSCLOS(doSubrule (true, mkAlts itm))
			| doItem (Syn.OPT itm) =
			    LLKSpec.OPT(doSubrule (true, mkAlts itm))
		      and doItems items = List.map doItem items
		      and mkAlts (Syn.SUBRULE alts) = alts
			| mkAlts (itm) = [Syn.ALT {
			    items = [itm],
			    action = NONE,
			    try = false,
			    pred = NONE
			  }]
		      and doSubrule (isEBNF, alts) = let
			    val prods = ref []
			    val sr = LLKSpec.NT{
				       name = Atom.atom (concat
						["SR", Int.toString (nextSRID())]),
				       formals = ref [],
				       binding = LLKSpec.WITHIN nt,
				       id = nextGlobalID(),
				       prods = prods,
				       isEBNF = isEBNF
				     }
		            in
			      loadNTerm(sr, [], alts);
			      insNTerm sr
		            end
		      val prod = LLKSpec.PROD{
			      lhs = nt,
			      rhs = doItems items,
			      id = nextGlobalID(),
			      name = Atom.atom (concat
				[Nonterm.name nt, "_PROD_", 
				 Int.toString (nextProdID())]),
			      action = Option.map Action.action action,
			      try = try,
			      pred = Option.map Action.action pred
			    }
		      in
			prodList := prod :: !prodList;	(* add to global prod list *)
			prods := prod :: !prods		(* add to lhs's prod list *)
		      end
		in
		  formals := newFormals;
		  List.app doAlt alts
		end
	(* check a rule *)
	  fun chkRule (Syn.RULE{lhs, formals, alts}) = loadNTerm(lookupNTerm lhs, formals, alts)
        (* check the grammar *)
	  val ((Syn.RULE{lhs, formals, alts})::rest) = 
	        (case rules
		  of [] => (Err.errMsg ["Error: no rules given."];
			    raise Err.Abort)
		   | _  => rules)
	  fun addEOF (Syn.ALT {items, action, try, pred}) = 
	        Syn.ALT {items = items @ [Syn.SYMBOL (Atom.atom "EOF", NONE)], 
			 action = action, try = try, pred = pred}
	  val fst' = Syn.RULE{lhs = lhs, formals = formals,
			      alts = map addEOF alts}
	  val _ = List.app chkRule (fst'::rest)
	(* check for undefined nonterminals, while reversing the order of productions *)
	  val _ = let
		fun chkNT (LLKSpec.NT{name, prods, ...}) = (case !prods
		       of [] => Err.errMsg ["Error: nonterminal ", Atom.toString name, " has no productions."]
			| l => prods := List.rev l
		      (* end case *))
		in
		  List.app chkNT (!ntList)
		end
	  val _ = Err.abortIfErr()
	  val nterms = rev(!ntList)
	  val startnt = hd (nterms)
	  in LLKSpec.Grammar {
	    header = header,
	    defs = Action.action defs,
	    toks = tokList,
	    nterms = nterms,
	    prods = List.rev(!prodList),
	    eof = lookupTok (Atom.atom "EOF"),
	    sortedTops = Nonterm.topsort startnt,
	    startnt = startnt,
	    actionStyle = actionStyle
	  } end

  end
