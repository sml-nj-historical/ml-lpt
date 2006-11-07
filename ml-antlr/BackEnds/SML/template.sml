structure Tok = struct

@tokens@


end (* structure Tok *)

signature LEXER = sig

  type strm
  val lex : strm -> (Tok.token * strm) option

end (* signature LEXER *)

signature REPAIRABLE = sig

  type T
  datatype repair
    = Deletion
    | Insertion of Tok.token
    | Substitution of Tok.token
  exception RepairableError

  val farEnoughWindow : {
	startAt : T,
	endAt : T
      } -> bool

  val farEnoughRepair : {
	startAt : T,
	endAt : T
      } -> bool

  val skip : T * int -> T 
  val isEmpty : T -> bool

  val chooseRepair : {
	startAt : T,
	endAt : T,
	try : T -> T
      } -> {
        errorAt : T, 
	repair : repair,
	repaired : T
      } option

end

@header@
(* : sig

    datatype repair_action
      = Insert of Tok.token list
      | Delete of Tok.token list
      | Subst of {
	    old : Tok.token list, 
	    new : Tok.token list
	}

    val parse : 

  end *) = struct

    structure UserCode = struct

@defs@

    end

    (* "wrapped" streams, which track the number of tokens read
     * and allow "prepending" a sequence of tokens
     *)
    structure WStream = struct

      datatype 'a wstream = WSTREAM of {
	prefix : Tok.token list,
	curTok : int,
	strm : 'a
      }

      fun wrap strm =  WSTREAM {prefix = [], strm = strm, curTok = 0}
      fun unwrap (WSTREAM {strm, ...}) = strm

      fun get1 (WSTREAM {prefix = tok::toks, strm, curTok}) = 
	    (tok, WSTREAM {prefix = toks, strm = strm, curTok = curTok + 1})
	| get1 (WSTREAM {prefix = [], strm, curTok}) = let
	    val (tok, strm') = case Lex.lex strm
				of SOME x => x
				 | NONE => (Tok.EOF, strm)
	    in (tok, WSTREAM {prefix = [], strm = strm', curTok = curTok + 1})
	    end

      fun prepend (toks, WSTREAM {prefix, strm, curTok}) = 
	    WSTREAM {prefix = toks @ prefix, strm = strm, curTok = curTok - (List.length toks)}

      fun subtract (WSTREAM {curTok = p1, ...}, WSTREAM {curTok = p2, ...}) = 
	    p1 - p2

      fun getDiff (ws1, ws2) =
	    if subtract (ws1, ws2) <= 0 then []
	    else let 
		val (t, ws2') = get1 ws2
	        in t :: (getDiff (ws1, ws2'))
                end

    end (* structure WStream *)
    
    structure EBNF = struct

      fun optional (pred, parse, strm) = 
	    if pred strm
    	    then let
	      val (y, strm') = parse strm
	      in 
		(SOME y, strm')
	      end
	    else (NONE, strm)

      fun closure (pred, parse, strm) = let
            fun iter (strm, ys) = 
	          if pred strm
		  then let
		    val (y, strm') = parse strm
		    in iter (strm', y::ys)
		    end
		  else (List.rev ys, strm)
            in
              iter (strm, [])
            end

      fun posclos (pred, parse, strm) = let
            val (y, strm') = parse strm
	    val (ys, strm'') = closure (pred, parse, strm')
            in
              (y::ys, strm'')
            end

    end (* structure EBNF *)

    structure RepairableStrm : REPAIRABLE = struct

      structure WS = WStream
      type T = Lex.strm WS.wstream
      exception RepairableError

      val minAdvance = 1

      fun farEnoughWindow {startAt, endAt} =
	    WS.subtract (endAt, startAt) > 15

      fun farEnoughRepair {startAt, endAt} =
	    WS.subtract (endAt, startAt) > minAdvance + 1

      datatype repair
	= Deletion
	| Insertion of Tok.token
	| Substitution of Tok.token

@repairs@


      fun applyRepair ([], repair) = 
	    raise Fail "applyRepair: expected nonempty working list"
	| applyRepair (working, Deletion) = tl working
	| applyRepair (working, Insertion tok) = tok :: working
	| applyRepair (working, Substitution tok) = tok :: (tl working)

      fun getWorking (strm, n, accum) = 
	    if n = 0 
	    then (strm, rev accum)
	    else let
	      val (tok, strm') = WS.get1 strm
	      in case tok
		  of Tok.EOF => (strm', rev (Tok.EOF :: accum))
		   | _ => getWorking (strm', n-1, tok::accum)
	      end

      fun skip (strm, 0) = strm
	| skip (strm, n) = skip (#2 (WS.get1 strm), n - 1)
      fun isEmpty strm = (case (#1 (WS.get1 strm))
			   of Tok.EOF => true
			    | _ => false)

      fun isKW _ = false (* TODO *)

      fun involvesKW (r, t) = (case r
            of Insertion t' => isKW t'
	     | Deletion => isKW t
	     | Substitution t' => isKW t orelse isKW t'
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

      fun chooseRepair {startAt, endAt, try} = let
	    val scoreOffset = WS.subtract (endAt, startAt)
	    val (endAt', working) = getWorking (startAt, scoreOffset + 5, [])
	    fun tryRepairs (prefix, working, repairs, best) = (case (working, repairs)
	      of ([], _) => (case best
			      of SOME (r, prefixLen, strm, _, _) => (*
print (case r
	of Deletion => "DEL\n"
	 | Insertion t => "INS " ^ Tok.toString t ^ "\n"
	 | Substitution t => "SUB " ^ Tok.toString t ^ "\n"); *)
				   SOME {
				     errorAt = skip (startAt, prefixLen),
				     repair = r,
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
		   val kw = involvesKW (r, hd working)
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
	    

    end (* structure RepairableStrm *)

    functor ErrHandlerFn(R : REPAIRABLE) : sig

      type err_handler
      val mkErrHandler : unit -> err_handler
      val whileDisabled : err_handler -> (unit -> 'a) -> 'a

      datatype repair 
	= Primary of {
	    errorAt : R.T,
	    repair : R.repair
          }
	| Secondary of {
	    deleteFrom : R.T,
	    deleteTo : R.T
	  }

(*      val wrap   : err_handler -> (R.T -> ('a * R.T)) -> R.T -> ('a * R.T) *)
      val wrap   : err_handler -> (R.T -> 'a) -> R.T -> 'a
      val launch : err_handler -> (R.T -> ('a * R.T)) -> 
		   R.T -> ('a * R.T * repair list)

    end = struct

      type repair_cont = R.T option SMLofNJ.Cont.cont 
      type retry_cont  = R.T        SMLofNJ.Cont.cont

      exception JumpOut of (R.T * retry_cont) list

      datatype repair 
	= Primary of {
	    errorAt : R.T,
	    repair : R.repair
          }
	| Secondary of {
	    deleteFrom : R.T,
	    deleteTo : R.T
	  }

      datatype err_handler = EH of {
	cont : repair_cont option ref, 
	enabled : bool ref,
	repairs : repair list ref
      }

      fun getCont    (EH {cont,    ...}) = !cont
      fun getEnabled (EH {enabled, ...}) = !enabled
      fun getRepairs (EH {repairs, ...}) = !repairs

      fun setCont    (EH {cont,    ...}, n) = cont := n
      fun setEnabled (EH {enabled, ...}, n) = enabled := n
      fun addRepair  (EH {repairs, ...}, n) = repairs := (!repairs) @ [n]

      fun mkErrHandler () = EH {cont = ref NONE, 
				enabled = ref true,
				repairs = ref []}
      fun whileDisabled eh f = let
	    val oldEnabled = getEnabled eh
            in
	      setEnabled (eh, false);
	      f () before setEnabled (eh, oldEnabled)
            end

      fun throwIfEH (eh, t) = 
	    if getEnabled eh then 
	      Option.app (fn k => SMLofNJ.Cont.throw k (SOME t)) (getCont eh)
	    else ()

      fun wrap eh f t = if getEnabled eh then let
	    val cont_ref : retry_cont option ref = ref NONE
	    val t' = SMLofNJ.Cont.callcc (fn k => (cont_ref := SOME k; t))
	    val retry = (t', valOf (!cont_ref))
            in
	      f t'
	      handle R.RepairableError => (
		       throwIfEH (eh, t');
		       raise JumpOut [retry])
		   | JumpOut stack => (
		       throwIfEH (eh, t');
		       raise JumpOut (retry::stack))
            end
          else f t

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
		   (* second time through, return the new right-most T *)
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
		of SOME {errorAt, repair, repaired} => 
		     SOME (Primary {errorAt = errorAt, repair = repair}, 
			   leftCont, repaired)
		 | NONE => NONE
            end

      fun secondaryRepair (eh, revStack) = let
	    val stack = rev revStack
	    val (errStrm, errCont) = hd stack
	    fun try ((strm, cont), strm', next) = let
	          val strm'' = tryRepair (eh, cont) strm'
	          in
	            if R.farEnoughRepair {startAt = strm', endAt = strm''}
		    then SOME (Secondary {
			   deleteFrom = strm,
			   deleteTo = strm'
			 }, cont, strm')
		    else next()
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
		    raise Fail "Unrecoverable parse error"
		  else leftRightRepair (R.skip (strm, 1), stack)
	      | leftRightRepair (strm, top::stack) = 
		  try (top, strm, fn () => leftRightRepair (strm, stack))
            in
	      case rightRepair (errStrm, 5)
	       of SOME r => r
		| _      => valOf (leftRightRepair (errStrm, []))
            end

      fun repair (eh, stack) = (case primaryRepair (eh, stack)
	    of SOME r => r
	     | NONE => secondaryRepair (eh, stack)
           (* end case *))

      fun launch eh f t = let
	    val (x, t') = wrap eh f t 
		handle JumpOut stack => let
		  val (r, cont, t') = repair (eh, stack)
		  in
		    addRepair (eh, r);
		    SMLofNJ.Cont.throw cont t'
		  end
            in
	      throwIfEH (eh, t');
	      (x, t', getRepairs eh)
            end

    end (* functor ErrHandlerFn *)

    structure Err = ErrHandlerFn(RepairableStrm)

    fun pretryProds eh prods strm = let
	fun try [] = raise RepairableStrm.RepairableError
	  | try (prod :: prods) = 
	      Err.whileDisabled eh (fn () => prod strm)
	      handle _ => try (prods)
        in
          try prods
        end

    exception ParseError = RepairableStrm.RepairableError

    fun innerParse
unwrapErr
@args@

strm = let
        val eh = Err.mkErrHandler()
	fun wrap f = Err.wrap eh f
	val launch = Err.launch eh
	val whileDisabled = Err.whileDisabled eh
	fun tryProds (strm, prods) = 
	      (wrap (pretryProds eh prods)) strm
	val lex = WStream.get1
@matchfns@

@parser@

        val (ret, strm', errors) = launch (parse'
@args@

) (WStream.wrap strm)
        in 
          (ret, strm', map unwrapErr errors)
        end

    datatype repair_action
      = Insert of Tok.token list
      | Delete of Tok.token list
      | Subst of {
	    old : Tok.token list, 
	    new : Tok.token list
	}

    structure R = RepairableStrm

    fun unwrapErr (Err.Primary {errorAt, repair = R.Deletion}) =
          (WStream.unwrap errorAt, Delete [(#1 (WStream.get1 errorAt))])
      | unwrapErr (Err.Primary {errorAt, repair = R.Insertion t}) =
          (WStream.unwrap errorAt, Insert [t])
      | unwrapErr (Err.Primary {errorAt, repair = R.Substitution t}) = 
          (WStream.unwrap errorAt, 
  	   Subst {
  	     old = [(#1 (WStream.get1 errorAt))],
  	     new = [t]
           })
      | unwrapErr (Err.Secondary {deleteFrom, deleteTo}) = 
          (WStream.unwrap deleteFrom, 
  	   Delete (WStream.getDiff (deleteTo, deleteFrom)))

    val parse = innerParse unwrapErr

    fun toksToString toks = String.concatWith " " (map Tok.toString toks)

    fun repairToString repair = (case repair
          of Insert toks => "inserting " ^ toksToString toks
	   | Delete toks => "deleting " ^ toksToString toks
	   | Subst {old, new} => 
	       "substituting " ^ toksToString new ^ " for "
	       ^ toksToString old
         (* end case *))

end (* structure Parser *)
