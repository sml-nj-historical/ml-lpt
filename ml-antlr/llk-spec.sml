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
	ty : ty option
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
      | WITHIN of nonterm

    and prod = PROD of {
	lhs : nonterm,
	rhs : item list,
	id : Int.int,
	name : Atom.atom,
	action : Action.action option,
	try : bool,
	pred : Action.action option
      }

    and item
      = TOK of token
      				(* nonterm * optional actual args *)
      | NONTERM of (nonterm * Action.action option)
      | CLOS of nonterm		(* ( ... )* *)
      | POSCLOS of nonterm	(* ( ... )+ *)
      | OPT of nonterm		(* ( ... )? *)

    withtype sem_pred = Action.action

    datatype action_style = datatype GrammarSyntax.action_style

    datatype grammar = Grammar of {
	defs : string,		(* user definitions *)
        toks : token list,
        nterms : nonterm list,
        prods : prod list,
	eof : token,
				(* topologically sorted nonterms *)
	sortedTops : nonterm list list,
	startnt : nonterm,
	actionStyle : action_style
      }

  end
