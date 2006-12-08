(* repairable-strm-sig.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Signature for "repairable streams," used in the code generated
 * by ml-antlr.
 *)

signature REPAIRABLE_STRM = sig

  type strm
  type token

  val farEnoughWindow : {
	startAt : strm,
	endAt   : strm
      } -> bool

  val tryDeletion : {
	oldStartAt : strm,
	startAt    : strm,
	endAt      : strm
      } -> token Repair.repair option

  val skip    : strm * int -> strm 
  val isEmpty : strm -> bool
  val getPos  : strm -> StreamPos.pos
  val get1    : strm -> token * StreamPos.span * strm

  val chooseRepair : {
	startAt  : strm,
	endAt    : strm,
	try      : strm -> strm
      } -> {
	repair   : token Repair.repair,
	repaired : strm
      } option

end