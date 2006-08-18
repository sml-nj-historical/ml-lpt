structure Tok = struct

@tokens@


end (* structure Tok *)

signature LEXER = sig

  type strm
  val get1 : strm -> Tok.token * strm

end (* signature LEXER *)

functor Parser(YY_Lex : LEXER) = struct

  structure YY = struct

@yydefs@

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

      fun get1 (WSTREAM {prefix = tok::toks, strm, curTok}) = 
	    (tok, WSTREAM {prefix = toks, strm = strm, curTok = curTok + 1})
	| get1 (WSTREAM {prefix = [], strm, curTok}) = let
	    val (tok, strm') = case YY_Lex.get1 strm
				of SOME x => x
				 | NONE => (Tok.EOF, strm)
	    in (tok, WSTREAM {prefix = [], strm = strm', curTok = curTok + 1})
	    end

      fun prepend (toks, WSTREAM {prefix, strm, curTok}) = 
	    WSTREAM {prefix = toks @ prefix, strm = strm, curTok - (List.length toks)}

      fun subtract (WSTREAM {curTok = p1, ...}, WSTREAM {curTok = p2, ...}) = 
	    p1 - p2

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
    
    signature REPAIRABLE = sig

      type T
      type repair
      exception RepairableError

      val farEnough : {
	    startAt : T,
	    endAt : T
	  } -> bool

      val chooseRepair : {
	    startAt : T,
	    endAt : T,
	    try : T -> T
	  } -> (T * repair) option

    end

    structure RepairableStrm : REPAIRABLE = struct

      structure WS = WStream
      type T = YY_Lex.strm WS.wstream

      fun farEnough {startAt, endAt} =
	    WS.subtract (endAt, startAt) > 15

      datatype repair
	= Deletion
	| Insertion of Tok.token
	| Substitution of Tok.token

      val minAdvance = 1

      fun applyRepair ([], repair) = 
	    raise Fail "applyRepair: expected nonempty working list"
	| applyRepair (working, Deletion) = tl working
	| applyRepair (working, Insertion tok) = tok :: working
	| applyRepair (working, Substitution tok) = tok :: (tl working)

      fun bestCand ([], _, NONE) = NONE
	| bestCand ([], _, SOME cand) = SOME cand
	| bestCand ( (c as (_, _, _, score))  ::cs, n, bs) = 
	    if score > n then
	      bestCand (cs, score, SOME c)
	    else bestCand (cs, n, bs)

      fun getWorking (strm, n, accum) = 
	    if n = 0 
	    then (strm, rev accum)
	    else let
	      val (tok, strm') = WS.get1 strm
	      in case tok
		  of Tok.EOF => (strm', rev (Tok.EOF :: accum))
		   | _ => getWorking (strm', n-1, tok::accum)
	      end

      fun chooseRepair {startAt, endAt, try} = let
	    fun tryRepairs (prefix, working, repairs, cands) = (case (working, repairs)
	      of ([], _) => (case bestCand (cands, 0, NONE)
			      of SOME (c as (r, _, strm, _)) => SOME (strm, r)
			       | NONE => NONE
			     (* end case *))
	       | (t::ts, []) => 
		   tryRepairs (prefix @ [t], ts, allRepairs, cands)
	       | (_, r::rs) => let
		   val strm = SW.prepend (prefix @ (applyRepair (working, r)), endAt)
		   val strm' = try strm
 		   val score = SW.subtract (strm', strm)
			         + (case r
				     of Deletion => 1
				      | Insertion _ => ~1
				      | Substitution _ => 0)
			         + scoreOffset
		   val cand = (r, List.length prefix, strm, score)
		   val cands' = if score > minAdvance then
				  cand::cands
				else cands
		   in
		     tryRepairs (prefix, working, rs, cands')
		   end
             (* end case *))
	    val working = getWorking (startAt, WS.subtract (endAt, startAt) + 5, [])
            in
	      tryRepairs ([], working, allRepairs, [])
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

      val wrap   : err_handler -> (R.T -> ('a, R.T)) -> R.T -> ('a, R.T)
      val launch : err_handler -> (R.T -> ('a, R.T)) -> 
		   R.T -> ('a, R.T, repair list)

    end = struct

      type repair_cont = R.T option SMLofNJ.Cont.cont 
      type retry_cont  = R.T        SMLofNJ.Cont.cont

      exception JumpOut of (R.T * retry_cont) list

      datatype err_handler = EH of {
	cont : repair_cont option ref, 
	enabled : bool ref
      }

      fun getCont    (EH {cont,    ...}) = !cont
      fun getEnabled (EH {enabled, ...}) = !enabled

      fun setCont    (EH {cont,    ...}, n) = cont := n
      fun setEnabled (EH {enabled, ...}, n) = enabled := n

      fun mkErrHandler () = EH {cont = ref NONE, enabled = ref true}
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
	    val rightMost = hd stack
	    fun TOf (t, _) = t
	    fun find [] = raise (Fail "BUG: findWindow given an empty stack")
	      | find [top] = (top, top)
	      | find (top::stack) = 
		  if R.farEnough {startAt = TOf top, endAt = TOf rightMost}
		  then (top, rightMost)
		  else find stack
            in
	      find revStack
            end

      fun tryRepair cont t = 
	    (case SMLofNJ.Cont.callcc (fn k => (eh := SOME k; NONE))
	      of NONE => 
		   (* first time through, try the repair *)
		   SMLofNJ.Cont.throw cont t		
	       | SOME t' => 
		   (* second time through, return the new right-most T *)
		   (eh := NONE; t')
	     (* end case *))

      fun primaryRepair (EH eh, stack) = let
	    val ((leftT, leftCont), (rightT, rightCont)) = 
		  findWindow stack
            in
	      R.chooseRepair {
	        startAt = leftT,
		endAt = rightT,
		try = tryRepair
	      }
            end

(*
      fun secondaryRepair (EH eh, stack) = let
            in
	      
            end
*)

      fun repair (eh, stack) = (case primaryRepair (eh, stack)
	    of SOME (t, r) => Primary {errorAt = t, repair = r}
	     | NONE        => raise Fail "todo" (* secondaryRepair (eh, stack) *)
           (* end case *))

      fun launch eh f t = 
	    if getEnabled eh then 
	      (f t handle R.JumpOut stack => repair (eh, stack))
	      before throwIfEH (eh, t)
	    else f t

    end (* functor ErrHandlerFn *)

    structure Err = ErrHandlerFn(RepairableStrm)

  end (* structure YY *)

@defs@

  val yylex = WS.get1
  fun yytryProds prods strm = let
	fun try [] = raise ParseError
	  | try (prod :: prods) = 
	      prod strm
	      handle _ => try (prods)
        in
          try prods
        end

  fun parse strm = let
        val yyeh = Err.mkErrHandler()
	val yywrap = Err.wrap yyeh
	val yylaunch = Err.launch yyeh
	val yywhileDisabled = Err.whileDisabled yyeh
        fun parse' strm = 
@parser@

        in 
          yylaunch (yywrap parse') (WS.wrap strm)
        end



(*
	  fun getDiff (strm, strm', accum) =
	        if SW.subtract (strm, strm') = 0 
		then rev accum
		else let
		  val (tok, strm'') = SW.get1 strm'
		  in getDiff (strm, strm'', tok::accum)
		  end
	  fun secondaryRepair (YY.ParseError {errStrm, errCont, revStack}) = let
	        val stack = (errStrm, errCont) :: (rev revStack)
		val _ = print (String.concat 
			  [" (Stack height: ", Int.toString (List.length stack), ")\n"])
		fun try (cont, strm, next) = 
		      (case SMLofNJ.Cont.callcc (fn k => (repairCont := SOME k; NONE))
			of NONE => SMLofNJ.Cont.throw cont strm
			 | SOME strm' => 
			     if SW.subtract (strm', strm) >= minAdvance + 2
			     then (repairCont := NONE; 
				   SMLofNJ.Cont.throw cont strm)
			     else next()
		       (* end case *))
		fun rightRepair (strm, n) = 
		      if n = 0 then ()
		      else let 
			val (_, tok, strm') = lex strm
			in 
			   print (String.concat ["Deleting ", Tok.toString tok, "\n"]);
			   try (errCont, strm', fn () => rightRepair (strm', n-1))
			end
		fun leftRightRepair (strm, []) = let
		      val (_, tok, strm') = lex strm
		      in case tok
			  of Tok.EOF => raise Fail "Unrecoverable parse error"
			   | _ => (print (String.concat 
				     ["Deleting ", Tok.toString tok, "\n"]);
				   leftRightRepair (strm', stack))
		      end
		  | leftRightRepair (strm, [(_, cont)]) = 
		      try (cont, strm, fn () => leftRightRepair (strm, []))
		  | leftRightRepair (strm, (bStrm1, _)::(stack as (bStrm2, bCont2)::_)) = let
		      val prefix = getDiff (bStrm1, bStrm2, [])
		      val strm' = SW.prepend (prefix, strm)
		      in
(*		        printStrm (10, strm'); print "\n"; *)
		        try (bCont2, strm', fn () => leftRightRepair (strm, stack))
		      end
		in
	          print " - Panic recovery, attempt 1 - \n";
	          rightRepair (errStrm, 5);
	          print " - Panic recovery, attempt 2 - \n";
		  leftRightRepair (errStrm, [])
	        end
*)

(*    fun parser strm = parser' (SW.wrap strm) *)

end (* structure Parser *)
