(* grammar-syntax.sml
 *
 * COPYRIGHT (c) 2005
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Parse tree for grammar input.
 *)

structure GrammarSyntax =
  struct

    type action = Int.int * String.string
    datatype action_style
      = ActNormal
      | ActDebug
      | ActUnit

    type symbol = Atom.atom

    datatype rule = RULE of {
	lhs : symbol,
	formals : Atom.atom list,
	alts : alt list
      }

    and alt = ALT of {
	items : (string option * item) list,
	action : action option,
	try : bool,
	pred : sem_pred option
      }

    and item
      = SYMBOL of symbol * action option
      | SUBRULE of alt list	(* ( ... ) *)
      | CLOS of item		(* ( ... )* *)
      | POSCLOS of item		(* ( ... )+ *)
      | OPT of item		(* ( ... )? *)

    withtype sem_pred = action

    type ty = string
    type constr = (Atom.atom * ty option * Atom.atom option)

    datatype import_change
      = ICDrop of symbol
      | ICReplace of rule
      | ICExtend of rule

    datatype grammar = GRAMMAR of {
        import : string option,
	importChanges : import_change list,
        name : string,
	defs : action,
	rules : rule list,
	toks : constr list,
	actionStyle : action_style,
	startSym : Atom.atom option,
	entryPoints : Atom.atom list,
	keywords : Atom.atom list,
	refcells : (string * ty * action) list
      }

    fun mkGrammar() = GRAMMAR {
	  import = NONE,
	  importChanges = [],
	  name = "Parser",
	  defs = (0, ""),
	  rules = [],
	  toks = [],
	  actionStyle = ActNormal,
	  startSym = NONE,
	  entryPoints = [],
	  keywords = [],
	  refcells = []
        }

    fun updName (g, new) = let
          val GRAMMAR {import, importChanges, name, defs, rules, toks, actionStyle, startSym, entryPoints, keywords, refcells} = g
          in GRAMMAR {import = import, importChanges = importChanges, name = new, defs = defs, rules = rules, 
		      toks = toks, actionStyle = actionStyle, startSym = startSym, entryPoints = entryPoints, keywords = keywords, refcells = refcells} end

    fun updDefs (g, new) = let
          val GRAMMAR {import, importChanges, name, defs, rules, toks, actionStyle, startSym, entryPoints, keywords, refcells} = g
          in GRAMMAR {import = import, importChanges = importChanges, name = name, defs = new, rules = rules, 
		      toks = toks, actionStyle = actionStyle, startSym = startSym, entryPoints = entryPoints, keywords = keywords, refcells = refcells} end

    fun updToks (g, new) = let
          val GRAMMAR {import, importChanges, name, defs, rules, toks, actionStyle, startSym, entryPoints, keywords, refcells} = g
          in GRAMMAR {import = import, importChanges = importChanges, name = name, defs = defs, rules = rules, 
		      toks = new, actionStyle = actionStyle, startSym = startSym, entryPoints = entryPoints, keywords = keywords, refcells = refcells} end

    fun updActionStyle (g, new) = let
          val GRAMMAR {import, importChanges, name, defs, rules, toks, actionStyle, startSym, entryPoints, keywords, refcells} = g
          in GRAMMAR {import = import, importChanges = importChanges, name = name, defs = defs, rules = rules, 
		      toks = toks, actionStyle = new, startSym = startSym, entryPoints = entryPoints, keywords = keywords, refcells = refcells} end

    fun debugAct g = updActionStyle (g, ActDebug)
    fun unitAct g = updActionStyle (g, ActUnit)

    fun addRule (g, new) = let
          val GRAMMAR {import, importChanges, name, defs, rules, toks, actionStyle, startSym, entryPoints, keywords, refcells} = g
          in GRAMMAR {import = import, importChanges = importChanges, name = name, defs = defs, rules = rules@[new], 
		      toks = toks, actionStyle = actionStyle, startSym = startSym, entryPoints = entryPoints, keywords = keywords, refcells = refcells} end

    fun addImportChange (g, new) = let
          val GRAMMAR {import, importChanges, name, defs, rules, toks, actionStyle, startSym, entryPoints, keywords, refcells} = g
          in GRAMMAR {import = import, importChanges = importChanges@[new], name = name, defs = defs, rules = rules, 
		      toks = toks, actionStyle = actionStyle, startSym = startSym, entryPoints = entryPoints, keywords = keywords, refcells = refcells} end

    fun updImport (GRAMMAR {import = NONE, importChanges, name, defs, rules, toks, actionStyle, startSym, entryPoints, keywords, refcells}, new) =
          GRAMMAR {import = SOME new, importChanges = importChanges, name = name, defs = defs, rules = rules, 
		   toks = toks, actionStyle = actionStyle, startSym = startSym, entryPoints = entryPoints, keywords = keywords, refcells = refcells}
      | updImport (g, _) = (Err.errMsg ["Error: multiple %imports are not allowed"]; g)

    fun updStartSym (GRAMMAR {import = import, importChanges, name, defs, rules, toks, actionStyle, startSym = NONE, entryPoints, keywords, refcells}, new) =
          GRAMMAR {import = import, importChanges = importChanges, name = name, defs = defs, rules = rules, 
		   toks = toks, actionStyle = actionStyle, startSym = SOME new, entryPoints = entryPoints, keywords = keywords, refcells = refcells}
      | updStartSym (g, _) = (Err.errMsg ["Error: multiple %start symbols are not allowed"]; g)

    fun updEntryPoints (GRAMMAR {import = import, importChanges, name, defs, rules, toks, actionStyle, startSym, entryPoints = [], keywords, refcells}, new) =
          GRAMMAR {import = import, importChanges = importChanges, name = name, defs = defs, rules = rules, 
		   toks = toks, actionStyle = actionStyle, startSym = startSym, entryPoints = new, keywords = keywords, refcells = refcells}
      | updEntryPoints (g, _) = (Err.errMsg ["Error: multiple %entry directives not allowed"]; g)

    fun updKeywords (GRAMMAR {import = import, importChanges, name, defs, rules, toks, actionStyle, startSym, entryPoints = entryPoints, keywords = [], refcells}, 
		     new) =
          GRAMMAR {import = import, importChanges = importChanges, name = name, defs = defs, rules = rules, 
		   toks = toks, actionStyle = actionStyle, startSym = startSym, entryPoints = entryPoints, keywords = new, refcells = refcells}
      | updKeywords (g, _) = (Err.errMsg ["Error: multiple %keywords directives not allowed"]; g)

    fun addRefcell (g, new) = let
          val GRAMMAR {import, importChanges, name, defs, rules, toks, actionStyle, startSym, entryPoints, keywords, refcells} = g
          in GRAMMAR {import = import, importChanges = importChanges, name = name, defs = defs, rules = rules, toks = toks, actionStyle = actionStyle, 
		      startSym = startSym, entryPoints = entryPoints, keywords = keywords, refcells = refcells@[new]} end

    fun setToTry (ALT {items, action, try, pred}) = 
	  ALT {items = items, action = action, try = true, pred = pred}

    fun addAction (ALT {items, action = NONE, try, pred}, act) = 
	  ALT {items = items, action = SOME act, try = try, pred = pred}
      | addAction _ = raise Fail "BUG: only one action allowed"

    fun addPred (ALT {items, action, try, pred = NONE}, pred) = 
	  ALT {items = items, action = action, try = try, pred = pred}
      | addPred _ = raise Fail "BUG: only one predicate allowed"

  end
