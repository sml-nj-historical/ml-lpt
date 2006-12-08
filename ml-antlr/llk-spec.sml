(* llk-spec.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Datatypes for grammar specification.
 * NOTE: individual modules are available that 
 *       manipulate the datatypes described in 
 *       this module.
 *)

structure LLKSpec =
  struct

    type ty = string

    datatype token = T of {
	id : Int.int,
	name : Atom.atom,
	ty : ty option,
	abbrev : Atom.atom option,
	keyword : bool
      }

    and nonterm = NT of {
	name : Atom.atom,
	binding : nt_binding,
	id : Int.int,
	prods : prod list ref,
	formals : Atom.atom list ref,
	isEBNF : bool
      }

    and nt_binding
      = TOP
      | WITHIN of prod

    and prod = PROD of {
	id : Int.int,
	name : Atom.atom,
	try : bool,
	lhs : nonterm,
	rhs : item list ref,
	rhsBindings : string list,
	pred : Action.action option,
	action : Action.action option
      }

    and preitem
      = TOK of token
      				(* nonterm * optional actual args *)
      | NONTERM of (nonterm * Action.action option)
      | CLOS of nonterm		(* ( ... )* *)
      | POSCLOS of nonterm	(* ( ... )+ *)
      | OPT of nonterm		(* ( ... )? *)

    and item = ITEM of {
	id : Int.int,
	sym : preitem
      }

    withtype sem_pred = Action.action

    datatype action_style = datatype GrammarSyntax.action_style
    type refcell = string * string * Action.action

    datatype grammar = Grammar of {
        name : string,
	defs : Action.action,	(* user definitions *)
        toks : token list,
        nterms : nonterm list,
        prods : prod list,
	eof : token,
				(* topologically sorted nonterms *)
	sortedTops : nonterm list list,
	startnt : nonterm,
	actionStyle : action_style,
	entryPoints : nonterm list,
	refcells : refcell list
      }

  end
