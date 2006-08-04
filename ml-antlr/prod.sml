(* prod.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Utility functions for the prod datatype.
 *)

structure Prod =
  struct

    datatype prod = datatype LLKSpec.prod

    fun toString (PROD{lhs, rhs, ...}) = concat[
	    (* "[", Int.toString id, "] ", *)
	    Nonterm.qualName lhs, " ::= ",
	    String.concatWith " " (List.map Item.toString rhs)
	  ]

    fun id (PROD{id, ...}) = id
    fun lhs (PROD{lhs, ...}) = lhs
    fun items (PROD{rhs, ...}) = rhs
    fun action (PROD{action, ...}) = action

    fun name (PROD{name, ...}) = Atom.toString name

    fun compare (p1, p2) = Int.compare(id p1, id p2)

    fun canTry (PROD{try = true, ...}) = true
      | canTry _ = false
(*
      | canTry (PROD{rhs, ...}) = 
	  List.exists (fn (LLKSpec.SEM_PRED _) => true
			| _ => false)
	              rhs
*)

    structure Set = RedBlackSetFn (
      struct
	type ord_key = prod
	val compare = compare
      end)

    structure Map = RedBlackMapFn (
      struct
	type ord_key = prod
	val compare = compare
      end)

    fun sortProds prods = 
	  ListMergeSort.sort 
	    (fn (x, y) => compare (x, y) = GREATER)
	    prods

  end
