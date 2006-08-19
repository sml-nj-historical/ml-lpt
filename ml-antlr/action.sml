(* action.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Simple (opaque) encapsulation of semantic actions.
 *)

structure Action :>
  sig

    type action

    val action : int * string -> action
    val toString : action -> string

    val same : (action * action) -> bool

  end = struct

    datatype action 
      = ACT of {
	  id : int,
	  code : string,
	  lineNo : int
        }

    local
      val cnt = ref 0
    in
    fun nextId() = (cnt := !cnt + 1; !cnt)
    end

    fun action (i, s) = ACT {id = nextId(), code = s, lineNo = i}
    fun toString (ACT {code, lineNo, ...}) = code
(*	  if lineNo = 1 then
	    "(*#line " ^ Int.toString lineNo ^ ".0*)" ^ code
	  else
	    "(*#line " ^ Int.toString (lineNo - 1) ^ ".0*) \n" ^ code
*)

    fun same (ACT {id = id1, ...}, ACT {id = id2, ...}) = (id1 = id2)

  end
