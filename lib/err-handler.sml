(* err-handler.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Error repair for ml-antlr
 *)

functor ErrHandler(R : REPAIRABLE_STRM) : sig

  exception RepairableError
  type 'a err_handler
  val mkErrHandler : { get : unit -> 'a,
		       put : 'a -> unit }
		     -> 'a err_handler
  val whileDisabled : 'b err_handler -> (unit -> 'a) -> 'a

(*      val wrap   : err_handler -> (R.strm -> ('a * R.strm)) -> R.strm -> ('a * R.strm) *)
  val wrap   : 'c err_handler -> (R.strm -> 'a) -> R.strm -> 'a
  val launch : 'c err_handler -> (R.strm -> ('a * 'b * R.strm)) -> 
	       R.strm -> ('a option * R.strm * R.token Repair.repair list)

  val tryProds : 'b err_handler -> (R.strm -> 'a) list -> R.strm -> 'a

end = struct

  type repair_cont = R.strm option SMLofNJ.Cont.cont 
  type retry_cont  = R.strm        SMLofNJ.Cont.cont

  exception RepairableError
  exception UnrepairableError
  exception JumpOut of (R.strm * retry_cont) list

  datatype 'a err_handler = EH of {
    cont : repair_cont option ref,
    enabled : bool ref,
    repairs : R.token Repair.repair list ref,
    get : unit -> 'a,
    put : 'a -> unit
  }

  fun getCont    (EH {cont,    ...}) = !cont
  fun getEnabled (EH {enabled, ...}) = !enabled
  fun getRepairs (EH {repairs, ...}) = !repairs
  fun getGet	 (EH {get,     ...}) = get
  fun getPut	 (EH {put,     ...}) = put
					   
  fun setCont    (EH {cont,    ...}, n) = cont := n
  fun setEnabled (EH {enabled, ...}, n) = enabled := n
  fun addRepair  (EH {repairs, ...}, n) = repairs := (!repairs) @ [n]

  fun mkErrHandler {get,put} = EH {
	cont = ref NONE, 
	enabled = ref true,
	repairs = ref [],
	get = get, put = put
      }

  fun whileDisabled eh f = let
        val oldEnabled = getEnabled eh
        in
	  setEnabled (eh, false);
	  (f () handle e => (setEnabled (eh, oldEnabled);
			     raise e))
	  before setEnabled (eh, oldEnabled)
        end

  fun throwIfEH (eh, t) = 
        if getEnabled eh then 
	  Option.app (fn k => SMLofNJ.Cont.throw k (SOME t)) (getCont eh)
	else ()

  fun wrap eh f t = if not (getEnabled eh) then f t else let
	val cont_ref : retry_cont option ref = ref NONE
	val state = (getGet eh) ()
	val t' = SMLofNJ.Cont.callcc (fn k => (cont_ref := SOME k; t))
	val retry = (t', valOf (!cont_ref))
        in
	  getPut eh state;
	  f t'
	  handle RepairableError => (
	    throwIfEH (eh, t');
	    raise JumpOut [retry])
	| JumpOut stack => (
	    throwIfEH (eh, t');
	    raise JumpOut (retry::stack))
        end

  fun findWindow (stack) = let
	val revStack = rev stack
	val rightMost = hd revStack
	fun TOf (t, _) = t
	fun find [] = raise (Fail "BUG: findWindow given an empty stack")
	  | find [top] = (top, rightMost)
	  | find (top::stack) = 
	      if R.farEnoughWindow {startAt = TOf top, endAt = TOf rightMost}
	      then (top, rightMost)
	      else find stack
        in
	  find revStack
        end

  fun tryRepair (eh, cont) t = 
        (case SMLofNJ.Cont.callcc (fn k => (setCont (eh, SOME k); NONE))
	  of NONE => 
	     (* first time through, try the repair *)
	       SMLofNJ.Cont.throw cont t
	   | SOME t' => 
	     (* second time through, return the new right-most strm *)
	       (setCont (eh, NONE); t')
	 (* end case *))

  fun primaryRepair (eh, stack) = let
	val ((leftT, leftCont), (rightT, rightCont)) = 
	    findWindow stack
	val repair = R.chooseRepair {
			startAt = leftT,
			endAt = rightT,
			try = tryRepair (eh, leftCont)
		     }
        in case repair
	    of SOME {repair, repaired} => 
	         SOME (repair, leftCont, repaired)
	     | NONE => NONE
        end

  fun secondaryRepair (eh, revStack) = let
	val stack = rev revStack
	val (errStrm, errCont) = hd stack
	fun try ((strm, cont), strm', next) = let
	      val strm'' = tryRepair (eh, cont) strm'
    	      in case (R.tryDeletion {oldStartAt = strm, 
				      startAt = strm', 
				      endAt = strm''})
		  of SOME r => SOME (r, cont, strm')
		   | NONE => next()
	      end
	fun rightRepair (strm, n) = 
	      if n = 0 then NONE
	      else let 
	        val strm' = R.skip (strm, 1)
		in 
		  try (hd stack, strm', fn () => rightRepair (strm', n-1))
		end
	fun leftRightRepair (strm, []) = 
	      if R.isEmpty strm then
		(addRepair (eh, (R.getPos errStrm, 
				 Repair.FailureAt (#1 (R.get1 errStrm))));
		 raise UnrepairableError)
	      else leftRightRepair (R.skip (strm, 1), stack)
	  | leftRightRepair (strm, top::stack) = 
	      try (top, strm, fn () => leftRightRepair (strm, stack))
        in case rightRepair (errStrm, 5)
	    of SOME r => r
	     | _      => valOf (leftRightRepair (errStrm, []))
        end

  fun repair (eh, stack) = (case primaryRepair (eh, stack)
	of SOME r => r
	 | NONE => secondaryRepair (eh, stack)
       (* end case *))

  fun launch eh f t = let
        val (x, _, t') = wrap eh f t 
	    handle JumpOut stack => let
	        val (r, cont, t') = repair (eh, stack)
		in
		  addRepair (eh, r);
		  SMLofNJ.Cont.throw cont t'
		end
        in
	  throwIfEH (eh, t');
	  (SOME x, t', getRepairs eh)
        end
    handle UnrepairableError =>
      (NONE, t, getRepairs eh)

  fun tryProds eh prods strm = let
	fun try [] = raise RepairableError
	  | try (prod :: prods) = let 
	      val state = (getGet eh) ()
	      in
	        whileDisabled eh (fn () => prod strm)
		handle _ => 
		  (getPut eh state;
		   try (prods))
	      end
        in
          try prods
        end

end