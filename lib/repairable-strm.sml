(* repairable-strm.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * "Repairable streams," used for error repair in
 * ml-antlr.
 *)

functor RepairableStrm (Tok : TOKENS) (Lex : LEXER) : sig

  type strm
  type token = Tok.token

  val wrap    : Lex.strm * (Lex.strm -> Tok.token * StreamPos.span * Lex.strm)
		-> strm
  val unwrap  : strm -> Lex.strm

  val get1    : strm -> Tok.token * StreamPos.span * strm
  val getPos  : strm -> StreamPos.pos
  val getSpan : strm -> StreamPos.span

  val skip    : strm * int -> strm 
  val isEmpty : strm -> bool

  val farEnoughWindow : {
	startAt : strm,
	endAt   : strm
      } -> bool

  val tryDeletion : {
	oldStartAt : strm,
	startAt    : strm,
	endAt      : strm
      } -> token Repair.repair option

  val chooseRepair : {
	startAt  : strm,
	endAt    : strm,
	try      : strm -> strm
      } -> {
	repair   : token Repair.repair,
	repaired : strm
      } option

end = struct

  structure WS = WrappedStrm(Tok)(Lex)
  type strm = WS.wstream
  type token = Tok.token

  val minAdvance = 1

  fun farEnoughWindow {startAt, endAt} =
        WS.subtract (endAt, startAt) > 15

  fun tryDeletion {oldStartAt, startAt, endAt} =
        if WS.subtract (endAt, startAt) > minAdvance + 1
	then SOME 
	   (WS.getPos oldStartAt, 
  	    Repair.Delete (map #1 (WS.getDiff (startAt, oldStartAt)))) 
	else NONE

  datatype repair
    = Deletion
    | Insertion of Tok.token
    | Substitution of Tok.token

  val allRepairs = [Deletion] @ (map Insertion    Tok.allToks) 
		              @ (map Substitution Tok.allToks) 

  fun applyRepair ([], repair) = 
        raise Fail "applyRepair: expected nonempty working list"
    | applyRepair (working, Deletion) = tl working
    | applyRepair ((t, span)::working, Insertion tok) = 
        (tok, (#2 span, #2 span)) :: (t, span) :: working
    | applyRepair ((_, span)::working, Substitution tok) = 
        (tok, span) :: working

  fun getWorking (strm, n, accum) = 
        if n = 0 
	then (strm, rev accum)
	else let
	  val (tok, s, strm') = WS.get1 strm
	  in if Tok.isEOF tok
	     then (strm', rev ((tok, WS.getSpan strm') :: accum))
	     else getWorking (strm', n-1, (tok, s)::accum)
	  end

  fun skip (strm, 0) = strm
    | skip (strm, n) = skip (#3 (WS.get1 strm), n - 1)
  fun isEmpty strm = Tok.isEOF (#1 (WS.get1 strm))

  fun involvesKW (r, t) = (case r
	of Insertion t' => Tok.isKW t'
	 | Deletion => Tok.isKW t
	 | Substitution t' => Tok.isKW t orelse Tok.isKW t'
       (* end case *))

  infix >>
  fun (Insertion _) >> _ = true
    | Deletion >> _ = true
    | _ >> _ = false

  fun chooseCand (c1, c2) = let
        val (r1, _, _, score1, kw1) = c1
	val (r2, _, _, score2, kw2) = c2
        in if score1 > score2 then c1
	   else if score2 > score1 then c2
	   else if r1 >> r2 then c1
	   else if r2 >> r1 then c2
	   else if kw1 = false then c1
	   else c2 
        end

  fun liftRepair (errorAt, Deletion) =
        (WS.getPos errorAt, 
	 Repair.Delete [(#1 (WS.get1 errorAt))])
    | liftRepair (errorAt, Insertion t) =
        (WS.getPos errorAt, 
	 Repair.Insert [t])
    | liftRepair (errorAt, Substitution t) = 
        (WS.getPos errorAt, 
  	 Repair.Subst {
	   old = [(#1 (WS.get1 errorAt))],
  	   new = [t]
         })

  fun chooseRepair {startAt, endAt, try} = let
        val scoreOffset = WS.subtract (endAt, startAt)
	val (endAt', working) = getWorking (startAt, scoreOffset + 5, [])
	fun tryRepairs (prefix, working, repairs, best) = (case (working, repairs)
	      of ([], _) => 
		   (case best
		     of SOME (r, prefixLen, strm, _, _) => 
			  SOME {
			    repair = liftRepair (skip (startAt, prefixLen), r),
			    repaired = strm
			  }
		      | NONE => NONE
		    (* end case *))
	       | (t::ts, []) => 
		   tryRepairs (prefix @ [t], ts, allRepairs, best)
	       | (_, r::rs) => let
		   val strm = WS.prepend (prefix @ (applyRepair (working, r)), endAt')
		   val strm' = try strm
 		   val score = WS.subtract (strm', strm)
			         + (case r
				     of Deletion => 1
				      | Insertion _ => ~1
				      | Substitution _ => 0)
			         - scoreOffset
		   val kw = involvesKW (r, #1 (hd working))
		   val cand = (r, List.length prefix, strm, score, kw)
		   val valid = if kw
			       then score > minAdvance + 2
			       else score > minAdvance 
		   val best' = if valid then 
				 case best
				  of NONE => SOME cand
				   | SOME c => SOME (chooseCand (c, cand))
			       else best
		   in
		     tryRepairs (prefix, working, rs, best')
		   end
             (* end case *))
        in
          tryRepairs ([], working, allRepairs, NONE)
        end    

  val wrap = WS.wrap
  val unwrap = WS.unwrap
  val get1 = WS.get1
  val getPos = WS.getPos
  val getSpan = WS.getSpan

end