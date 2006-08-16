structure Tok = 
  struct

@tokens@


  end

structure Parser =
  struct

  structure YY = struct

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

      fun get1 get (WSTREAM {prefix = tok::toks, strm, curTok}) = 
	    (tok, WSTREAM {prefix = toks, strm = strm, curTok = curTok + 1})
	| get1 get (WSTREAM {prefix = [], strm, curTok}) = let
	    val (tok, strm') = case get strm
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

    functor ErrHandlerFn(R : REPAIRABLE) : sig

      type err_handler
      val mkErrHandler : unit -> err_handler
      val whileDisabled : err_handler -> (unit -> 'a) -> 'a

      val wrap   : err_handler -> (R.T -> ('a, R.T)) -> R.T -> ('a, R.T)
      val launch : err_handler -> (R.T -> ('a, R.T)) -> 
		   R.T -> ('a, R.T, R.repair list)

    end = struct

      type repair_cont = R.T option SMLofNJ.Cont.cont 
      type retry_cont  = R.T        SMLofNJ.Cont.cont

      datatype err_handler = EH of repair_cont option ref

      exception JumpOut of (R.T * retry_cont) list

      fun throwIfEH (EH eh, t) = 
	    Option.app (fn k => SMLofNJ.Cont.throw k (SOME t)) (!eh)

      fun wrap eh f t = let
	    val retry as (t', cont) = SMLofNJ.Cont.callcc (fn k => (t, k))
            in
	      f t'
	      handle R.RepairableError => (
		       throwIfEH (eh, t');
		       raise JumpOut [retry])
		   | JumpOut stack => (
		       throwIfEH (eh, t');
		       raise JumpOut (retry::stack))
            end

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

      fun primaryRepair (EH eh, stack) = let
	    val ((leftT, leftCont), (rightT, rightCont)) = 
		  findWindow stack
	    fun tryRepair t = 
		  (case SMLofNJ.Cont.callcc (fn k => (eh := SOME k; NONE))
		    of NONE => SOME t
		     | SOME t' => SOME t'
		   (* end case *))
            in
	      R.chooseRepair {
	        startAt = leftT,
		endAt = rightT,
		try = tryRepair
	      }
            end

      fun secondaryRepair (EH eh, stack) = let
            in
	      
            end

      fun repair (eh, stack) = (case primaryRepair (eh, stack)
	    of SOME t => t
	     | NONE   => secondaryRepair (eh, stack)
           (* end case *))
	    

      fun launch eh f t = 
	    (f t handle R.JumpOut stack => repair (eh, stack))
	    before throwIfEH (eh, t)

      datatype repair
	= Deletion
	| Insertion of Tok.token
	| Substitution of Tok.token

@yydefs@


    end

@defs@

    val parse = let
          val repairCont : YY.stream option SMLofNJ.Cont.cont option ref = ref NONE
	  val minAdvance = 1
	  fun lex strm = let
	        val cont : YY.stream SMLofNJ.Cont.cont option ref = ref NONE
		val (tok, strm') = 
		    SW.get1 (SMLofNJ.Cont.callcc (fn k => (cont := SOME k; strm)))
		val exn = YY.ParseError {
			    errStrm = strm,
			    errCont = valOf (!cont),
			    revStack = []
			  }
	        in
	          (exn, tok, strm')
	        end
	  fun applyRepair ([], repair) = 
	        raise Fail "applyRepair: expected nonempty working list"
	    | applyRepair (working, YY.Deletion) = tl working
	    | applyRepair (working, YY.Insertion tok) = tok :: working
	    | applyRepair (working, YY.Substitution tok) = tok :: (tl working)
	  fun printStrm (n, strm) = 
	        if n = 0 then ()
		else let
		  val (_, tok, strm') = lex strm
		  in
		    print (Tok.toString tok); print " ";
		    printStrm (n-1, strm')
		  end
	  fun printCand (repair, pos, strm, score) = (print (String.concat [
	        (case repair
		  of YY.Deletion => "DEL "
		   | YY.Insertion tok => "INS " ^ (Tok.toString tok)
		   | YY.Substitution tok => "SUB " ^ (Tok.toString tok)),
		" @ ", Int.toString pos, " ==> ", Int.toString score, "\n"]);
		printStrm (pos + 5, strm); print "\n")
	  fun bestCand ([], _, NONE) = NONE
	    | bestCand ([], _, SOME cand) = SOME cand
	    | bestCand ( (c as (_, _, _, score))  ::cs, n, bs) = 
	        if score > n then
		  bestCand (cs, score, SOME c)
		else bestCand (cs, n, bs)
	  fun tryRepairs (prefix, working, repairs, strm, resume, cands, scoreOffset) = 
	        (case (working, repairs)
		  of ([], _) => (case bestCand (cands, 0, NONE)
                       of SOME (c as (_, _, strm, _)) =>
		            (printCand c; print "\n";
			     SOME strm)
			| NONE => NONE
		      (* end case *))
		   | (t::ts, []) => 
		       tryRepairs 
			 (prefix @ [t], ts, YY.allRepairs, strm, resume, cands, scoreOffset)
		   | (_, r::rs) => let
		       val strm' = SW.prepend (prefix @ (applyRepair (working, r)), strm)
		       in 
		         case SMLofNJ.Cont.callcc (fn k => (repairCont := SOME k; NONE))
			  of NONE => SOME strm'
			   | SOME strm'' => let
			       val score = SW.subtract (strm'', strm') +
					   (case r
					     of YY.Deletion => 1
					      | YY.Insertion _ => ~1
					      | YY.Substitution _ => 0) +
					   scoreOffset
			       val cand = (r, List.length prefix, strm', score)
			       val cands' = if score > minAdvance then
					      cand::cands
					    else cands
			       in
			         repairCont := NONE;
				 tryRepairs 
				   (prefix, working, rs, strm, resume, cands', scoreOffset)
			       end
		       end
		 (* end case *))
	  fun getWorking (strm, n, accum) = 
	        if n = 0 
		then (strm, rev accum)
		else let
		  val (tok, strm') = SW.get1 strm
		  in case tok
		      of Tok.EOF => (strm', rev (Tok.EOF :: accum))
		       | _ => getWorking (strm', n-1, tok::accum)
		  end
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
          fun handleError (atBottom, exn, curStrm, resume) = 
	      (case !repairCont
                of NONE => resume (SMLofNJ.Cont.callcc (fn cont => let
		     val exn' = YY.addToStack (exn, curStrm, cont)
		     in if not atBottom
			then raise exn'
			else let
val _ = print " --- syntax error ---\n"
			  val (errStrm, backStrm, offset) = YY.findWindow exn'
			  val (errStrm', working) = getWorking (backStrm, offset + 5, [])
			  val res = tryRepairs ([], working, YY.allRepairs,
						errStrm', resume, [], ~offset)
			  in case res
			      of SOME strm => strm
			       | NONE => secondaryRepair exn'
			  end
		     end))
		 | SOME k => let
		     val YY.ParseError {errStrm, ...} = exn
		     in 
		       SMLofNJ.Cont.throw k (SOME errStrm)
		     end
               (* end case *))
	  fun tryProds (strm, prods) = let
	    val oldRepairCont = !repairCont
	    val strmCont : YY.stream SMLofNJ.Cont.cont option ref = ref NONE
	    val strm' = SMLofNJ.Cont.callcc (fn k => (strmCont := SOME k; strm))
	    val exn = YY.ParseError {
		        errStrm = strm,
			errCont = valOf (!strmCont),
			revStack = []
		      }
	    fun try [] = (repairCont := oldRepairCont; raise exn)
	      | try (prod :: prods) = 
		  (case SMLofNJ.Cont.callcc (fn k => (repairCont := SOME k; NONE))
		    of NONE => prod strm'
		     | SOME _ => try (prods)
                   (* end case *))
            in
	      try prods
	      before repairCont := oldRepairCont
            end

	  fun parser' strm = 
@parser@

	  handle (YY.ParseError e) =>
            handleError (true, YY.ParseError e, strm, parser')

          in (fn strm => let
		   val _ = repairCont := NONE
		   val (result, strm') = parser' (SW.wrap strm)
		   in case !repairCont
		       of NONE => (result, strm')
			| SOME k => SMLofNJ.Cont.throw k (SOME strm')
		   end)
          end

(*    fun parser strm = parser' (SW.wrap strm) *)

  end
