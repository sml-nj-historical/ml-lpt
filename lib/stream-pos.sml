(* stream-pos.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Very simple position tracking and source maps for ml-ulex/ml-antlr
 *)

structure StreamPos :> sig

  type pos = Position.int
  type span = pos * pos
  type sourcemap

  (* the result of moving forward an integer number of characters *)
  val forward : pos * int -> pos

  val mkSourcemap : unit -> sourcemap
  val same : sourcemap * sourcemap -> bool

  (* log a new line occurence *)
  val markNewLine : sourcemap -> pos -> unit

  val lineNo   : sourcemap -> pos -> int
  val colNo    : sourcemap -> pos -> int
  val toString : sourcemap -> pos -> string

end = struct

  type pos = Position.int
  type span = pos * pos
  type sourcemap = (int * int) list ref

  fun forward (p, i) = p + (Position.fromInt i)

  fun mkSourcemap() = ref []
  fun same (sm1, sm2) = (sm1 = sm2)

  fun markNewLine sm newPos = let
        val (curLine, pos) = case !sm
			      of x::_ => x
			       | nil => (1, ~1)
        in
          if pos < newPos then
	    sm := (curLine + 1, newPos)::(!sm)
	  else ()
        end

  fun findLB ((lineNo, pos)::sm, pos') = 
        if pos <= pos' then (lineNo, pos)
	else findLB(sm, pos')
    | findLB _ = (1, ~1)

  fun lineNo sm pos = #1 (findLB(!sm, pos))
  fun colNo  sm pos = pos - (#2 (findLB(!sm, pos)))
  fun toString sm pos = String.concat [
	"[", Int.toString (lineNo sm pos), ".",
	     Int.toString (colNo  sm pos), "]"]

end