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

    val action : string -> action
    val toString : action -> string

    val same : (action * action) -> bool

  end = struct

    datatype action 
      = ACT of Int.int * string

    local
      val cnt = ref 0
    in
    fun nextId() = (cnt := !cnt + 1; !cnt)
    end

    fun action s = ACT (nextId(), s)
    fun toString (ACT (_, s)) = s

    fun same (ACT (id1, _), ACT (id2, _)) = (id1 = id2)

  end
