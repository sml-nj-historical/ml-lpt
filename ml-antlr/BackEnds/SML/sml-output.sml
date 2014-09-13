(* sml-output.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Back end for SML code, using first-class continuations for
 * Burke-Fisher-style error repair/recovery
 *)

structure SMLOutput = 
  struct

    structure S = LLKSpec
    structure P = Predict

    structure TMap = Token.Map
    structure TSet = Token.Set
    structure NMap = Nonterm.Map

    structure NT = Nonterm

    datatype ml_exp = datatype ML.ml_exp
    datatype ml_pat = datatype ML.ml_pat
    datatype ml_decl = datatype ML.ml_decl
    datatype ml_fundecl = datatype ML.ml_fundecl
    datatype ml_fun_heading = datatype ML.ml_fun_heading

  (* the following functions compute names or small expressions
   * used throughout the backend 
   *)
   
    fun NTFnName nt = NT.name nt ^ "_NT"
    fun NTFnVar nt = ML_Var (NTFnName nt)

    fun predFnName nt = NT.name nt ^ "_PRED"

    fun tokConName tok = "Tok." ^ Token.name tok
    fun tokConVar tok = ML_Var (tokConName tok)
    fun tokConPat tok = ML_ConPat (tokConName tok, 
			    if Token.hasTy tok
			    then [ML_Wild]
			    else [])
    fun tokConPat' tok = ML_ConPat (Token.name tok, 
			    if Token.hasTy tok
			    then [ML_Wild]
			    else [])
    fun tokMatch' tok = "match" ^ (Token.name tok)
    fun tokMatch tok = "" ^ tokMatch' tok

    fun tokExpected tok = ML_Raw [ML.Tok ("raise Fail \"expected " ^ (Token.name tok) ^ "\"")]

    val bindingSuffix = "_RES"
    val spanSuffix = "_SPAN"
    val fullSpan = "FULL_SPAN"
    val spanTySuffix = " : (Lex.pos * Lex.pos)"
    val rcSuffix = "_REFC"

    fun actionHeader (name, (bindings, formals), suffix, isPred, refcells, refSuffix) = let
	  val withSuffix = map (fn b => Atom.toString b ^ suffix) 
			       (AtomSet.listItems (AtomSet.union (bindings, formals)))
	  val withSpan   = map (fn b => Atom.toString b ^ spanSuffix ^ spanTySuffix) 
			       (AtomSet.listItems bindings)
	  val refs = map (fn (S.REFCELL {name, ...}) => name ^ refSuffix) refcells
	  val args = if isPred then 
		       withSuffix @ refs
		     else
		       withSuffix @ withSpan @ [fullSpan ^ spanTySuffix] @ refs
          in
            String.concat [name, " (", 
			   String.concatWith ", " args,
			   ")"]
          end

  (* make an expression that will pull the next token off the stream *)
    fun mkGet1 strm = ML_App ("lex", [ML_Var (strm)])
  (* make an expression that will pull the kth token off the stream *)
    fun mkGetk (strm, 1) = mkGet1 strm
      | mkGetk (strm, k) = ML_App ("lex", [ML_App ("#2", [mkGetk (strm, k-1)])])

    fun rawCode code = ML_Raw [ML.Tok code]

  (* make an expression for the given (polymorphic) decision tree *)
    fun mkPredict (pickFn, choiceFn, strm, tree, errAction) = let
          fun mkPredict (strm, P.Pick p) = 
	        pickFn p
	    | mkPredict (strm, P.ByTok branches) = let
		val branches = List.concat (map mkMatch branches)
		val errCase = (ML_Wild, errAction)
	        in
	          ML_Case (mkGet1 strm, branches @ [errCase])
	        end
	    | mkPredict (strm, P.Choice prods) = 
	        choiceFn prods
	  and mkMatch (set, tree) = 
	        map (fn tok => (ML_TupPat [tokConPat tok, ML_Wild, ML_VarPat "strm'"],
				mkPredict ("strm'", tree)))
		    (TSet.listItems set)
          in
            mkPredict (strm, tree)
          end

  (* make a production *)
    fun mkProd (grm, pm) prod = let
          val rhs = Prod.items prod
	  val S.Grammar {refcells, ...} = grm
	  fun mkTok (t, strmExp, letFn) = 
	        letFn (ML_App (tokMatch t, [strmExp]))
	  fun mkNT (nt, strmExp, args, letFn, item) = let
	        val name = case (args, !Options.actStyle)
		  of (SOME args, Options.ActNormal) => 
		       "(" ^ NTFnName nt ^ " ("
		       ^ actionHeader 
			   ("UserCode.ARGS_" ^ Action.name args, 
			    Item.bindingsLeftOf (item, prod), 
			    bindingSuffix, true, refcells, rcSuffix)
		       ^ "))"
		   | _ => NTFnName nt
	        val innerExp = ML_App (name, [strmExp])
	        in
	          if NT.isSubrule nt
		  then letFn (mkNonterm (grm, pm) (nt, innerExp))
		  else letFn innerExp
	        end
	  fun mkEBNF (nt, strmExp, fname, letFn) = let
	        val predName = predFnName nt
	        val innerExp = letFn (ML_App (fname, [ML_Var predName, ML_Var (NTFnName nt), strmExp]))
		val Predict.PMaps {ebnfPredict, ...} = pm
		val predTree = ebnfPredict nt
		fun mkBool true = ML_Var "true"
		  | mkBool false = ML_Var "false"
		fun choiceFn _ = raise Fail "BUG: mkEBNF: backtracking choice unexpected"
		val errAction = ML_Var "false"
		val caseExp = mkPredict (mkBool, choiceFn, "strm", predTree, errAction)
		val predFn = ML_Funs ([(predName, ["strm"], caseExp)], innerExp)
		in 
	          mkNonterm (grm, pm) (nt, predFn)
	        end
	  fun mkItem strm ((item, binding), k) = let
	        val strmExp = ML_Var strm
		fun mkLet e = ML_Let (String.concat 
		      ["(", binding, bindingSuffix, 
		       ", ", binding, spanSuffix, ", strm')"], 
		      e, k)
	        in
	          case Item.sym item
		   of S.TOK t      => mkTok  (t,  strmExp, mkLet)
		    | S.NONTERM (nt, args)
				   => mkNT   (nt, strmExp, args, mkLet, item)
		    | S.CLOS nt    => mkEBNF (nt, strmExp, "EBNF.closure", mkLet)
		    | S.POSCLOS nt => mkEBNF (nt, strmExp, "EBNF.posclos", mkLet)
		    | S.OPT nt     => mkEBNF (nt, strmExp, "EBNF.optional", mkLet)
	        end
	  val itemBindings = Prod.itemBindings prod
	  val action = 
	      case !Options.actStyle
	       of Options.ActDebug =>
		  "( print \"" ^ (Nonterm.qualName (Prod.lhs prod)) ^ "\\n\" )"
		| Options.ActUnit => "()"
		| Options.ActNormal => (case Prod.action prod
		    of SOME _ => actionHeader ("UserCode." ^ Prod.fullName prod ^ "_ACT", 
					       Prod.bindingsAtAction prod, bindingSuffix, false, 
					       refcells, rcSuffix)
		     | NONE => let
			 val bindings = 
			       List.mapPartial 
				 (fn (binding, hasValue) => 
				     if hasValue 
				     then SOME (binding ^ bindingSuffix)
				     else NONE)
				 (ListPair.zip (itemBindings, Prod.itemYields prod))
			 in 
			   if List.length bindings > 0 
			   then "(" ^ (String.concatWith ", " bindings) ^ ")"
			   else "()"
		         end
  	           (* end case *))
	  fun innerExp strm = let
	        val strmVar = ML_Var (strm)
	        val span = if List.length itemBindings = 0 then
			     ML_Tuple [ML_App ("Err.getPos", [strmVar]),
				       ML_App ("Err.getPos", [strmVar])]
			   else
			     ML_Tuple [ML_App ("#1", [ML_Var (hd itemBindings ^ spanSuffix)]),
				       ML_App ("#2", [ML_Var (hd (rev itemBindings) ^ spanSuffix)])]
	        val act = ML_Tuple [ML_Raw [ML.Tok action], ML_Var fullSpan, strmVar]
		val spanExp = ML_Let (fullSpan, span, act)
	        in case (Prod.pred prod, !Options.actStyle)
		    of (SOME pred, Options.ActNormal) =>
		         ML_If (ML_Raw [ML.Tok ("(" 
				  ^ actionHeader
				      ("UserCode." ^ Prod.fullName prod ^ "_PRED",
				       Prod.bindingsAtAction prod, bindingSuffix, true,
				       refcells, rcSuffix)
				  ^ ")")], 
				spanExp,
				ML_App ("fail", []))
		     | _ => spanExp
	        end
	  val parse = case (ListPair.zip (rhs, itemBindings))
		       of [] => innerExp "strm"
			| fst::rst => 
			    mkItem "strm" 
			      (fst, List.foldr (mkItem "strm'") (innerExp "strm'") rst)
          in 
            parse
          end

  (* make a group of productions, along with a decision tree to choose one of them *)
    and mknProds (grm, pm, nt) = let
	  fun mkProdFun (prod, k) = ML_Funs ([(Prod.name prod, ["strm"], 
					    mkProd (grm, pm) prod)], k)
	  val Predict.PMaps {prodPredict, ...} = pm
	  val tree = prodPredict nt
	  fun pickFn prod = ML_App (Prod.name prod, [ML_Var "strm"])
	  fun choiceFn prods = 
	        ML_App ("tryProds", [ML_Var "strm", 
					ML_List (map (ML_Var o Prod.name) prods)])
	  val errAction = ML_App ("fail", [])
	  val caseExp = mkPredict (pickFn, choiceFn, "strm", tree, errAction)
          in
	    foldr mkProdFun caseExp (Nonterm.prods nt)
          end

    and mkNonterm' (grm, pm) nt = let
          val formals = case !Options.actStyle
	      of Options.ActNormal =>
	        if length (Nonterm.formals nt) > 0
		then " (" ^ (String.concatWith ", " 
			       (map 
				  (fn f => Atom.toString f ^ bindingSuffix)
				  (Nonterm.formals nt)))
		     ^ ")"
		else ""
	       | _ => ""
	  val exp = if List.length (Nonterm.prods nt) = 1
		    then mkProd (grm, pm) (hd (Nonterm.prods nt))
		    else mknProds(grm, pm, nt)
          in 
            (NTFnName nt ^ formals, ["strm"], exp)
          end
    and mkNonterm (grm, pm) (nt, k) = ML_Funs ([mkNonterm' (grm, pm) nt], k)

    fun mkNonterms (grm, pm) (nts, k) = 
	  ML_Funs (map (mkNonterm' (grm, pm)) nts, k)

  (* output the main parser body *)
    fun parserHook spec strm = let
          val (grm as S.Grammar {toks, nterms, startnt, sortedTops, entryPoints, ...}, 
	       pm) = spec
          val ppStrm = TextIOPP.openOut {dst = strm, wid = 80}
	  val entries = map NTFnName (startnt :: entryPoints)
	  val entriesVal = "val (" ^ String.concatWith ", " entries ^ ") = "
	  val innerExp = ML_Tuple (map ML_Var entries)
	  val parser = List.foldl (mkNonterms (grm, pm)) innerExp sortedTops
	  fun optParam nt = if length (Nonterm.formals nt) > 0 then " x " else " "
	  fun optParamFn nt = if length (Nonterm.formals nt) > 0 then " fn x => " else " "
	  fun wrWrapParse nt = TextIO.output (strm, String.concat [
		"val ", NTFnName nt, " = ", optParamFn nt, 
		"fn s => unwrap (Err.launch (eh, lexFn, ",
		NTFnName nt, optParam nt, ", ",
		if Nonterm.same (nt, startnt) then "true" else "false",
		") s)\n"])
	  fun wrEntry (name, nt) = TextIO.output (strm, String.concat [
		"fun ", name, " lexFn ", optParam nt,
		"s = let ", entriesVal, "mk lexFn in ", NTFnName nt,
		if length (Nonterm.formals nt) > 0
		  then  " x "
		  else " ",
		"s end\n\n"])
		  
          in
            TextIO.output (strm, entriesVal ^ "\n");
            ML.ppML (ppStrm, parser);
	    TextIO.output (strm, "\n");
	    app wrWrapParse (startnt::entryPoints);
            TextIO.output (strm, "\nin ("
	      ^ String.concatWith ", " entries
	      ^ ") end\n");
	    TextIO.output (strm, "  in\n");
	    wrEntry ("parse", startnt);
	    app (wrEntry o (fn x => ("parse" ^ Nonterm.name x, x))) entryPoints;
	    TextIO.output (strm, "  end\n")
          end

  (* make a match function for a token *)
    fun ppMatch (strm, ppStrm) t = let
          val matchCase = 
	        (ML_TupPat 
		   [ML_ConPat (tokConName t,
			       if Token.hasTy t
			       then [ML_VarPat "x"]
			       else []),
		    ML_VarPat "span",
		    ML_VarPat "strm'"],
		 if Token.hasTy t
		 then ML_Tuple [ML_Var "x", ML_Var "span", ML_Var "strm'"]
		 else ML_Tuple [ML_Var "()", ML_Var "span", ML_Var "strm'"])
	  val errCase = (ML_Wild, ML_App ("fail", []))
          val exp = ML_Case (mkGet1 "strm", [matchCase, errCase])
	  in
            TextIO.output (strm, "fun " ^ tokMatch' t ^ " strm = ");
	    ML.ppML (ppStrm, exp);
	    TextIO.output (strm, "\n")
          end

  (* output the tokens datatype and related functions *)
    fun tokensHook spec strm = let
          val (S.Grammar {toks, ...}, _) = spec
          val ppStrm = TextIOPP.openOut {dst = strm, wid = 80}
	  val toksDT = 
	        "    datatype token = "
		^ (String.concatWith "\n      | " (List.map Token.def toks))
        (* list of all nullary and default token values for error repair *)
	  val allToks = let
		fun make (tok, ts) = if Token.hasTy tok
		      then (case Token.default tok
			 of SOME arg => concat[Token.name tok, "(", arg, ")"] :: ts
			  | NONE => ts
			(* end case *))
		      else Token.name tok :: ts
		in
		  List.foldr make [] toks
		end
	(* toString function *)
	  fun mkMat t = (ML_TupPat [tokConPat' t], rawCode (Token.quoted t))
          val casesExp = ML_Case (ML_Var "tok", List.map mkMat toks)
        (* isKW function *)
	  fun mkKWMat t = (ML_TupPat [tokConPat' t], 
			   ML_Var (if Token.isKW t then "true" else "false"))
	  val kwCasesExp = ML_Case (ML_Var "tok", List.map mkKWMat toks)
          in
            TextIO.output (strm, toksDT ^ "\n\n");
	    TextIO.output (strm, "    val allToks = [");
	    TextIO.output (strm, String.concatWith ", " allToks);
	    TextIO.output (strm, "]\n\n");
            TextIO.output (strm, "    fun toString tok =\n");
	    ML.ppML (ppStrm, casesExp);
	    TextIO.output (strm, "\n");
            TextIO.output (strm, "    fun isKW tok =\n");
	    ML.ppML (ppStrm, kwCasesExp);
	    TextIO.output (strm, "\n")
          end

    fun matchfnsHook spec strm = let
          val (S.Grammar {toks, ...}, _) = spec
          val ppStrm = TextIOPP.openOut {dst = strm, wid = 80}
          in
	    app (ppMatch (strm, ppStrm)) toks
          end

  (* output header *)
    fun headerHook (grm, _) strm = (case grm
	   of S.Grammar{header = SOME h, ...} => TextIO.output (strm, h)
	    | S.Grammar{name, ...} =>
		TextIO.output (strm, String.concat [
		    "functor ", name, "ParseFn(Lex : ANTLR_LEXER)"
		  ])
	  (* end case *))

  (* output tokens module name *)
    fun tokmodHook spec strm = let
          val (S.Grammar {name, ...}, _) = spec
          in
            TextIO.output (strm, name ^ "Tokens")
          end

  (* output user definitions *)
    fun defsHook spec strm = let
          val (S.Grammar {defs, ...}, _) = spec
	  fun output ss = TextIO.output (strm, String.concat ss)
          in
            TextIO.output (strm, Action.toString defs)
          end

  (* output user actions *)
    fun actsHook spec strm = let
          val (S.Grammar {prods, refcells, ...}, _) = spec
	  fun output ss = TextIO.output (strm, String.concat ss)
	  fun actionLevel (suffix, f, isPred) prod = (case f prod
            of SOME code => output [
	         "fun ", 
		 actionHeader (
		   Prod.fullName prod ^ suffix, 
		   Prod.bindingsAtAction prod, "", isPred, refcells, ""), 
		 " = \n  (", Action.toString code, ")",
		  (case Nonterm.ty (Prod.lhs prod)
		    of NONE => ""
		     | SOME ty => " : " ^ ty),
		 "\n"]
	     | NONE => ())
	  fun args prod (itm as S.ITEM {sym = S.NONTERM (nt, SOME code), ...}) = 
	        output ["fun ",
		  actionHeader (
		    "ARGS_" ^ Action.name code,
		    Item.bindingsLeftOf (itm, prod), "", true, refcells, ""),
		  " = \n  (", Action.toString code, ")\n"]
	    | args _ _ = ()
	  fun outCell (S.REFCELL {name, initCode, ty, ...}) = TextIO.output (strm, 
		String.concat [
		  "fun mk", name ^ rcSuffix, "() : (", ty, ") ref = ref (", 
		  Action.toString initCode, ")\n"])
          in
	    app (actionLevel ("_ACT", Prod.action, false)) prods;
	    app (actionLevel ("_PRED", Prod.pred, true)) prods;
	    app (fn prod => app (args prod) (Prod.items prod)) prods;
	    app outCell refcells
          end

    fun ehargsHook spec strm = let
	  fun out s = TextIO.output (strm, s)
          val (S.Grammar {refcells, ...}, _) = spec
	  val names = map (fn (S.REFCELL {name, ...}) => name) refcells
	  fun prepend pre s = pre ^ s
	  fun append post s = s ^ post
	  fun mkc (S.REFCELL {name, ...}) = String.concat [
		"val ", name, rcSuffix, " = UserCode.mk", name, rcSuffix, "()\n"]
          in
            app (out o mkc) refcells;
	    out (String.concat ["fun getS() = {",
		   String.concatWith ", "
		     (map (fn nm => nm ^ " = !" ^ nm ^ rcSuffix)
			  names),
		   "}\n"]);
	    out (String.concat ["fun putS{",
		   String.concatWith ", " names,
		   "} = (",
		   String.concatWith
		     "; " (map (fn nm => nm ^ rcSuffix ^ " := " ^ nm)
			       names),
		   ")\n"]);
	    out (String.concat ["fun unwrap (ret, strm, repairs) = ",
		   "(ret, strm, repairs",
		   if List.length names > 0 then ", getS()" else "",
		   ")"])
          end

    fun output (grm, pm, fname) = 
          ExpandFile.expandTemplate {
	      src = SMLTemplate.template,
	      dst = fname ^ ".sml",
	      hooks = [("parser",   parserHook (grm, pm)),
		       ("tokens",   tokensHook (grm, pm)),
		       ("tokmod",   tokmodHook (grm, pm)),
		       ("header",   headerHook (grm, pm)),
		       ("usrdefs",  defsHook (grm, pm)),
		       ("actions",  actsHook (grm, pm)),
		       ("ehargs",   ehargsHook (grm, pm)),
		       ("matchfns", matchfnsHook (grm, pm))]
	    }

  end
