(* item.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Utility code for the item datatype.
 *)

structure Item =
  struct

    structure S = LLKSpec

    fun toString (S.TOK t) = Token.toString t
      | toString (S.NONTERM (nt, args)) = 
	  String.concat ([Nonterm.toString nt] @ (
	    case args
	     of SOME args =>
		["@(", Action.toString args, ")"]
	      | _ =>  []))
      | toString (S.CLOS nt) = Nonterm.toString nt ^ "*"
      | toString (S.POSCLOS nt) = Nonterm.toString nt ^ "+"
      | toString (S.OPT nt) = Nonterm.toString nt ^ "?"

    fun listToString l = String.concatWith " " (map toString l)

    fun name (S.TOK t) = Token.name t
      | name (S.NONTERM (nt, _)) = Nonterm.name nt
      | name (S.CLOS nt) = Nonterm.name nt
      | name (S.POSCLOS nt) = Nonterm.name nt
      | name (S.OPT nt) = Nonterm.name nt

    fun binding (S.TOK t) = if Token.hasTy t then SOME (Token.name t) else NONE
      | binding (S.NONTERM (nt, _)) = SOME (Nonterm.name nt)
      | binding (S.CLOS nt)	    = SOME (Nonterm.name nt)
      | binding (S.POSCLOS nt)	    = SOME (Nonterm.name nt)
      | binding (S.OPT nt)	    = SOME (Nonterm.name nt)

  (* Item IDs; we shift the ID and use the low bit to
   * get a globally unique ID.
   *)
    local
      val tokenTag = 0w0
      val ntermTag = 0w1
      fun tagID (id, tag) = Word.toIntX(Word.orb(Word.<<(Word.fromInt id, 0w1), tag))
    in
    fun itemID (S.TOK(S.T{id, ...})) = tagID(id, tokenTag)
      | itemID (S.NONTERM(S.NT{id, ...}, _)) = tagID(id, ntermTag)
      | itemID (S.CLOS(S.NT{id, ...})) = tagID(id, ntermTag)
      | itemID (S.POSCLOS(S.NT{id, ...})) = tagID(id, ntermTag)
      | itemID (S.OPT(S.NT{id, ...})) = tagID(id, ntermTag)
    end (* local *)

    fun compare (item1, item2) = Int.compare(itemID item1, itemID item2)

    structure Set = RedBlackSetFn (
      struct
	type ord_key = S.item
	val compare = compare
      end)

    structure Map = RedBlackMapFn (
      struct
	type ord_key = S.item
	val compare = compare
      end)

  end
