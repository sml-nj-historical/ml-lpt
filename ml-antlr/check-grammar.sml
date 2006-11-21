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
    structure S = LLKSpec
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

  (* load the %tokens definition into a tokTbl : atom -> LLKSpec.token *)
    fun loadToks (nextGlobalID, toks, keywords) = let
          val kwSet = ref (AtomSet.addList (AtomSet.empty, keywords))
	  val tokTbl = ATbl.mkTable (64, Fail "token table")
	  val tokList = ref []
	  fun tryKW (name) = 
	        if AtomSet.member (!kwSet, name) 
		then (true before kwSet := AtomSet.delete (!kwSet, name))
		else false
	  fun addTok (name, ty, abbrev) = (case ATbl.find tokTbl name
		 of NONE => let
		      val id = nextGlobalID()
		      val isKW = tryKW name orelse 
				 (case abbrev
				   of SOME a => tryKW a
				    | NONE => false)
		      val info = S.T{name = name, id = id, ty = ty, 
				     abbrev = abbrev, keyword = isKW}
		      in
			ATbl.insert tokTbl (name, info); 
			Option.app (fn a => ATbl.insert tokTbl (a, info)) abbrev;
			tokList := info :: !tokList
		      end
		  | SOME info => Err.errMsg ["Error: token '", Atom.toString name, 
					     "' defined multiple times."]
		(* end case *))
	  fun checkNonTokKW () = 
	        if AtomSet.isEmpty (!kwSet) then ()
		else Err.errMsg ["Error: ", 
		  String.concatWith " " (map Atom.toString (AtomSet.listItems (!kwSet))),
		  " appears in %keywords directive but not in %tokens directive."]
          in
            List.app addTok toks;
	    checkNonTokKW();
	    (List.rev (!tokList), tokTbl)
          end

    structure AMap = AtomMap

  (* recursively import grammars, applying changes (%drop, %replace, %extend)
   * as the grammar is imported.  note that grammars are integrated directly
   * from the parse tree form (GrammarSyntax.grammar) rather than the processed
   * form that the check function below produces (LLKSpec.grammar).
   *)
    fun appImport (Syn.GRAMMAR {import = SOME file, importChanges, header, defs, 
				rules, toks, actionStyle, startSym, entryPoints, keywords}) = let
	  val Syn.GRAMMAR {rules = prules, ...} = appImport (ParseFile.parse file)
	  fun ins (rule as Syn.RULE{lhs, ...}, map) =
	        if AMap.inDomain (map, lhs) then
		  (Err.errMsg ["Error [", file, "]: ", Atom.toString lhs, " is multiply defined."];
		   map)
		else AMap.insert (map, lhs, rule)
	  fun tryFind (lhs, map, err, f) = (case AMap.find (map, lhs)
		of NONE =>
		  (Err.errMsg ["Error: cannot ", err, " ", Atom.toString lhs, 
			       " because it is not defined in the parent grammar."];
		   map)
		 | SOME rule => f rule
	       (* end case *))
	  fun appChg (Syn.ICDrop lhs, map) =
	        tryFind (lhs, map, "drop", fn _ => 
		  #1 (AMap.remove (map, lhs)))
	    | appChg (Syn.ICExtend (rule as Syn.RULE{lhs, alts, formals}), map) = 
	        tryFind (lhs, map, "extend", fn (Syn.RULE{alts = palts, ...}) =>
		  AMap.insert (map, lhs, 
		    Syn.RULE{lhs = lhs, alts = palts@alts, formals = formals}))
	    | appChg (Syn.ICReplace (rule as Syn.RULE{lhs, ...}), map) = 
	        tryFind (lhs, map, "replace", fn _ => AMap.insert (map, lhs, rule))
	  val map = foldl ins AMap.empty prules
	  val map' = foldl appChg map importChanges
          in
            Syn.GRAMMAR {import = SOME file, importChanges = importChanges, header = header,
			 defs = defs, rules = (AMap.listItems map')@rules, toks = toks, 
			 actionStyle = actionStyle, startSym = startSym, entryPoints = entryPoints,
			 keywords = keywords}
          end
      | appImport (g as Syn.GRAMMAR {importChanges = [], ...}) = g
      | appImport g = (Err.errMsg ["Error: import alterations (%drop, %extend...) ",
				   "cannot be used unless %import is specified"];
		       g)

  (* auto-number the bindings for repeated variable names *)
    local
      fun addNumbers(acc, _, _, []) = rev acc
	| addNumbers(acc, n, s, s'::bs) = 
	    if s = s' then 
	      addNumbers((s' ^ (Int.toString n))::acc, n+1, s, bs)
	    else addNumbers(s'::acc, n, s, bs)
      fun numberBindings([]) = []
	| numberBindings(b::bs) = 
	    if List.exists (fn b' => b = b') bs
	    then (b ^ "1")::numberBindings(addNumbers([], 2, b, bs))
	    else b::(numberBindings bs)
      (* assign a binding to an item *)
      fun binding tokTbl (Syn.SYMBOL (name, _)) = (case ATbl.find tokTbl name
	    of SOME tok => Token.name tok
	     | NONE => Atom.toString name
           (* end case *))
	| binding tokTbl (Syn.SUBRULE _)	= "SR"
	| binding tokTbl (Syn.CLOS itm)		= binding tokTbl itm
	| binding tokTbl (Syn.POSCLOS itm)	= binding tokTbl itm
	| binding tokTbl (Syn.OPT itm)		= binding tokTbl itm
    in
    (* assign bindings to a list of items *)
    fun bindings (userNames, items, tokTbl) = 
	  ListPair.map getOpt
	    (userNames, numberBindings (map (binding tokTbl) items))
    end

  (* check a GrammarSyntax.grammar value for errors, while transforming
   * it into an LLKSpec.grammar suitable for analysis and parser generation.
   *)
    fun check (g : Syn.grammar) = let
	  val _ = Err.status "checking grammar"
	  val nextGlobalID = nextId (ref 0)
	(* inherit any %import-ed grammars, and apply all modifications to 
	 * imported nonterminal definitions
	 *)
	  val Syn.GRAMMAR {header, defs, rules, toks, actionStyle, 
			   startSym, entryPoints, keywords, ...} = 
	        appImport g  
	  val _ = if List.length toks = 0 then
		    Err.errMsg ["Error: no tokens defined."]
		  else ()
	(* load the tokens.  note that EOF is implicitly defined *)
          val (tokList, tokTbl) = 
	        loadToks (nextGlobalID, (Atom.atom "EOF", NONE, NONE)::toks, keywords)
	  fun lookupTok name = (case ATbl.find tokTbl name
		 of NONE => (
		      Err.errMsg ["Token ", Atom.toString name, " is undefined"];
		      lookupTok (Atom.atom "EOF")) (* return EOF as a default token *)
		  | SOME info => info
		(* end case *))
	  val eofTok = lookupTok (Atom.atom "EOF")
	(* add EOF to the end of start symbol *)
	  val startSymName = 
	        case startSym
		 of SOME sym => sym
		  | NONE => (case rules
			      of (Syn.RULE {lhs, ...})::_ => lhs
			       | _ => Atom.atom "EOF")
	  fun addEOFToAlt (Syn.ALT {items, action, try, pred}) = 
	        Syn.ALT {items = items @ [(NONE, Syn.SYMBOL (Atom.atom "EOF", NONE))], 
			 action = action, try = try, pred = pred}
	  fun addEOFToRule (r as Syn.RULE {lhs, formals, alts}) = 
	        if Atom.same (lhs, startSymName) then
		  Syn.RULE{lhs = lhs, formals = formals,
			   alts = map addEOFToAlt alts}
		else r
	  val rules = map addEOFToRule rules
	(* keep track of nonterminals *)
	  val numNTerms = ref 0
	  val ntTbl = ATbl.mkTable (64, Fail "nonterm table")
	  val ntList = ref []
	  fun insNTerm (nt as S.NT{name, ...}) =
	        (ATbl.insert ntTbl (name, nt);
		 ntList := nt :: !ntList;
		 nt)
	(* map a non-terminal name to its info record, creating a new nonterminal 
	 * record if none is found.
	 *)
	  fun lookupNTerm name = (case ATbl.find ntTbl name
		 of NONE => insNTerm (S.NT{name = name, id = nextGlobalID(), formals = ref [],
						 binding = S.TOP, prods = ref[], isEBNF = false}) 
		  | SOME info => info
		(* end case *))
	(* keep track of productions *)
	  val prodList = ref []
	(* check a nonterminal *)
          fun loadNTerm(nt, newFormals, alts) = let
(* val _ = print(concat["chkRule: ", Atom.toString lhs, "\n"]); *)
		val S.NT{prods, formals, ...} = nt
		val nextProdID = nextId (ref 1)
             (* check an alternative, creating a production *)
		fun doAlt (rhs) = let
		      val Syn.ALT {items, action, try, pred} = rhs
		      val (userNames, items) = ListPair.unzip items
		      val prodrhs = ref []
		      val prod = S.PROD{
			      lhs = nt,
			      rhs = prodrhs,
			      rhsBindings = bindings (userNames, items, tokTbl),
			      id = nextGlobalID(),
			      name = Atom.atom (concat
				[Nonterm.name nt, "_PROD_", 
				 Int.toString (nextProdID())]),
			      action = Option.map Action.action action,
			      try = try,
			      pred = Option.map Action.action pred
			    }
		      val nextSRID = nextId (ref 1)
		      fun doPreitem (Syn.SYMBOL (name, args)) = 
			    if ATbl.inDomain tokTbl name
			    then if not (isSome args)
			         then S.TOK(lookupTok name)
			         else (
				   Err.errMsg ["Attempted to apply arguments to token ",
					       Atom.toString name, "."];
				   S.TOK eofTok)
			    else S.NONTERM(lookupNTerm name, 
					   Option.map Action.action args)				 
			| doPreitem (Syn.SUBRULE alts) =
			    S.NONTERM(doSubrule (false, alts), NONE)
			| doPreitem (Syn.CLOS itm) =
			    S.CLOS(doSubrule (true, mkAlts itm))
			| doPreitem (Syn.POSCLOS itm) =
			    S.POSCLOS(doSubrule (true, mkAlts itm))
			| doPreitem (Syn.OPT itm) =
			    S.OPT(doSubrule (true, mkAlts itm))
		      and doItem s = S.ITEM {sym = doPreitem s, 
					     id = nextGlobalID()}
		      and mkAlts (Syn.SUBRULE alts) = alts
			| mkAlts (itm) = [Syn.ALT {
			    items = [(NONE, itm)],
			    action = NONE,
			    try = false,
			    pred = NONE
			  }]
		      and doSubrule (isEBNF, alts) = let
			    val prods = ref []
			    val sr = S.NT{
				       name = Atom.atom (concat
						["SR", Int.toString (nextSRID())]),
				       formals = ref [],
				       binding = S.WITHIN prod,
				       id = nextGlobalID(),
				       prods = prods,
				       isEBNF = isEBNF
				     }
		            in
			      loadNTerm(sr, [], alts);
			      insNTerm sr
		            end
		      in
			prodrhs := map doItem items;	(* compute RHS *)
			prodList := prod :: !prodList;	(* add to global prod list *)
			prods := prod :: !prods		(* add to lhs's prod list *)
		      end
		in
	          if List.length (!prods) > 0 
		    then Err.errMsg ["Error: duplicate definition for nonterminal ",
				     Nonterm.name nt, "."]
		    else ();
		  formals := newFormals;
		  List.app doAlt alts
		end
	(* check a rule *)
	  fun chkRule (Syn.RULE{lhs, formals, alts}) = loadNTerm(lookupNTerm lhs, formals, alts)
        (* check the grammar *)
	  val _ = if List.length rules = 0 then (
		    Err.errMsg ["Error: no rules defined."];
		    raise Err.Abort)
		  else ()
	  val _ = app chkRule rules
	(* check for undefined nonterminals, while reversing the order of productions *)
	  val _ = let
		fun chkNT (S.NT{name, prods, ...}) = (case !prods
		       of [] => Err.errMsg ["Error: symbol ", Atom.toString name, " is not defined."]
			| l => prods := List.rev l
		      (* end case *))
		in
		  List.app chkNT (!ntList)
		end
	  val nterms = rev(!ntList)
	(* node: safe to assume length nterms > 0, otherwise aborted above *)
	  fun findNT errStr sym = (case ATbl.find ntTbl sym
		of NONE => (Err.errMsg ["Error: ", errStr, " symbol ", 
					Atom.toString sym,
					" is not defined."];
		            hd nterms)
		 | SOME nt => nt
	       (* end case *))
	  val startnt = case startSym
			 of NONE => hd nterms
			  | SOME sym => findNT "%start" sym
	  val entryPoints' = map (findNT "%entry") entryPoints
	  val sortedTops = Nonterm.topsort (startnt::entryPoints')
	  val topsSet = AtomSet.addList 
			  (AtomSet.empty, 
			   map (Atom.atom o Nonterm.name) (List.concat sortedTops))
	  fun checkNTInTops nt = 
	        if Nonterm.isSubrule nt = false then
		  if AtomSet.member (topsSet, Atom.atom (Nonterm.name nt)) = false 
		  then Err.warning ["Warning: nonterminal ", Nonterm.name nt,
				    " is not reachable from any entry point."]
		  else ()
		else ()
	  val _ = app checkNTInTops nterms
	  val _ = Err.abortIfErr()
	  in S.Grammar {
	    header = header,
	    defs = Action.action defs,
	    toks = tokList,
	    nterms = nterms,
	    prods = List.rev(!prodList),
	    eof = eofTok,
	    sortedTops = sortedTops,
	    startnt = startnt,
	    actionStyle = actionStyle,
	    entryPoints = entryPoints'
	  } end

  end
