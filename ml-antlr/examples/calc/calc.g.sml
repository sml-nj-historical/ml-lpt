structure Tok = struct

    datatype token = EOF
      | DummyExp of int
      | SEMI
      | RP
      | LP
      | MINUS
      | TIMES
      | PLUS
      | EQ
      | NUM of Int.int
      | ID of string
      | KW_in
      | KW_let

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (DummyExp(_)) => "DummyExp"
  | (SEMI) => ";"
  | (RP) => ")"
  | (LP) => "("
  | (MINUS) => "-"
  | (TIMES) => "*"
  | (PLUS) => "+"
  | (EQ) => "="
  | (NUM(_)) => "NUM"
  | (ID(_)) => "ID"
  | (KW_in) => "in"
  | (KW_let) => "let"
(* end case *))


end (* structure Tok *)

signature LEXER = sig

  type strm
  type pos
  type span = pos * pos

  val lex : strm -> (Tok.token * span * strm) option
  val getPos : strm -> pos

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

functor CalcParse(Lex : LEXER)(* : sig

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

      type antlr_annotation = unit



fun exp_PROD_1_ACT (EQ, ID, EOF, env, exp1, exp2, KW_in, KW_let, EQ_SPAN : Lex.span, ID_SPAN : Lex.span, EOF_SPAN : Lex.span, exp1_SPAN : Lex.span, exp2_SPAN : Lex.span, KW_in_SPAN : Lex.span, KW_let_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  exp2 )
fun addExp_PROD_1_ACT (SR, env, multExp, SR_SPAN : Lex.span, multExp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  List.foldl op+ multExp SR )
fun multExp_PROD_1_ACT (SR, env, prefixExp, SR_SPAN : Lex.span, prefixExp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  List.foldl op* prefixExp SR )
fun prefixExp_PROD_2_ACT (env, MINUS, prefixExp, MINUS_SPAN : Lex.span, prefixExp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  ~prefixExp )
fun atomicExp_PROD_1_ACT (ID, env, ID_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  valOf(AtomMap.find (env, Atom.atom ID)) )
fun atomicExp_PROD_1_PRED (ID, env) = 
  (  AtomMap.inDomain (env, Atom.atom ID) )
fun ARGS_1 () = 
  (AtomMap.empty)
fun ARGS_3 (EQ, ID, env, KW_let) = 
  (env)
fun ARGS_4 (EQ, ID, env, exp1, KW_in, KW_let) = 
  (AtomMap.insert(env, Atom.atom ID, exp1))
fun ARGS_5 (env) = 
  (env)
fun ARGS_8 (env, PLUS, multExp) = 
  (env)
fun ARGS_7 (env) = 
  (env)
fun ARGS_11 (env, TIMES, prefixExp) = 
  (env)
fun ARGS_10 (env) = 
  (env)
fun ARGS_12 (env) = 
  (env)
fun ARGS_14 (env, MINUS) = 
  (env)
fun ARGS_17 (LP, env) = 
  (env)

    end

    (* "wrapped" streams, which track the number of tokens read
     * and allow "prepending" a sequence of tokens
     *)
    structure WStream = struct

      datatype 'a wstream = WSTREAM of {
	prefix : (Tok.token * Lex.span) list,
	curTok : int,
	strm : 'a
      }

      fun wrap strm =  WSTREAM {prefix = [], strm = strm, curTok = 0}
      fun unwrap (WSTREAM {strm, ...}) = strm

      fun get1 (WSTREAM {prefix = (tok, span)::toks, strm, curTok}) = 
	    (tok, span, WSTREAM {prefix = toks, strm = strm, curTok = curTok + 1})
	| get1 (WSTREAM {prefix = [], strm, curTok}) = let
	    val (tok, span, strm') = case Lex.lex strm
		of SOME x => x
		 | NONE => (Tok.EOF, (Lex.getPos strm, Lex.getPos strm), strm)
	    in (tok, span, WSTREAM {prefix = [], strm = strm', curTok = curTok + 1})
	    end

      fun prepend (toks, WSTREAM {prefix, strm, curTok}) = 
	    WSTREAM {prefix = toks @ prefix, strm = strm, curTok = curTok - (List.length toks)}

      fun subtract (WSTREAM {curTok = p1, ...}, WSTREAM {curTok = p2, ...}) = 
	    p1 - p2

      fun getDiff (ws1, ws2) =
	    if subtract (ws1, ws2) <= 0 then []
	    else let 
		val (t, s, ws2') = get1 ws2
	        in (t, s) :: (getDiff (ws1, ws2'))
                end

      fun getPos ws = let val (_, (left, _), _) = get1 ws in left end
      fun getSpan ws = (getPos ws, getPos ws)

    end (* structure WStream *)
    
    structure EBNF = struct

      fun optional (pred, parse, strm) = 
	    if pred strm
    	    then let
	      val (y, span, strm') = parse strm
	      in 
		(SOME y, span, strm')
	      end
	    else (NONE, WStream.getSpan strm, strm)

      fun closure (pred, parse, strm) = let
            fun iter (strm, (left, right), ys) = 
	          if pred strm
		  then let
		    val (y, (_, right'), strm') = parse strm
		    in iter (strm', (left, right'), y::ys)
		    end
		  else (List.rev ys, (left, right), strm)
            in
              iter (strm, WStream.getSpan strm, [])
            end

      fun posclos (pred, parse, strm) = let
            val (y, (left, _), strm') = parse strm
	    val (ys, (_, right), strm'') = closure (pred, parse, strm')
            in
              (y::ys, (left, right), strm'')
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

    val allRepairs = [Deletion, Insertion Tok.EOF, Insertion Tok.SEMI, Insertion Tok.RP, Insertion Tok.LP, Insertion Tok.MINUS, Insertion Tok.TIMES, Insertion Tok.PLUS, Insertion Tok.EQ, Insertion Tok.KW_in, Insertion Tok.KW_let, Substitution Tok.EOF, Substitution Tok.SEMI, Substitution Tok.RP, Substitution Tok.LP, Substitution Tok.MINUS, Substitution Tok.TIMES, Substitution Tok.PLUS, Substitution Tok.EQ, Substitution Tok.KW_in, Substitution Tok.KW_let]


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
	      in case tok
		  of Tok.EOF => (strm', rev ((Tok.EOF, WStream.getSpan strm') :: accum))
		   | _ => getWorking (strm', n-1, (tok, s)::accum)
	      end

      fun skip (strm, 0) = strm
	| skip (strm, n) = skip (#3 (WS.get1 strm), n - 1)
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
	    

    end (* structure RepairableStrm *)

    functor ErrHandlerFn(R : REPAIRABLE) : sig

      type err_handler
      val mkErrHandler : unit -> err_handler
      val addAnnotation : err_handler -> UserCode.antlr_annotation -> unit
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
      val launch : err_handler -> (R.T -> ('a * 'b * R.T)) -> 
		   R.T -> ('a * R.T * repair list * UserCode.antlr_annotation list)

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
	repairs : repair list ref,
	annotations : UserCode.antlr_annotation list ref
      }

      fun getCont    (EH {cont,    ...}) = !cont
      fun getEnabled (EH {enabled, ...}) = !enabled
      fun getRepairs (EH {repairs, ...}) = !repairs
      fun getAnns    (EH {annotations, ...}) = !annotations

      fun setCont    (EH {cont,    ...}, n) = cont := n
      fun setEnabled (EH {enabled, ...}, n) = enabled := n
      fun addRepair  (EH {repairs, ...}, n) = repairs := (!repairs) @ [n]
      fun setAnns    (EH {annotations, ...}, n) = annotations := n

      fun mkErrHandler () = EH {cont = ref NONE, 
				enabled = ref true,
				repairs = ref [],
			        annotations = ref []}

      fun addAnnotation eh a = setAnns (eh, a :: (getAnns eh))

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
	    val anns = getAnns eh
	    val t' = SMLofNJ.Cont.callcc (fn k => (cont_ref := SOME k; t))
	    val retry = (t', valOf (!cont_ref))
            in
	      setAnns (eh, anns);
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
	    val (x, _, t') = wrap eh f t 
		handle JumpOut stack => let
		  val (r, cont, t') = repair (eh, stack)
		  in
		    addRepair (eh, r);
		    SMLofNJ.Cont.throw cont t'
		  end
            in
	      throwIfEH (eh, t');
	      (x, t', getRepairs eh, getAnns eh)
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

    datatype repair_action
      = Insert of Tok.token list
      | Delete of Tok.token list
      | Subst of {
	    old : Tok.token list, 
	    new : Tok.token list
	}

    structure R = RepairableStrm

    fun unwrapErr (Err.Primary {errorAt, repair = R.Deletion}) =
          (WStream.getPos errorAt, Delete [(#1 (WStream.get1 errorAt))])
      | unwrapErr (Err.Primary {errorAt, repair = R.Insertion t}) =
          (WStream.getPos errorAt, Insert [t])
      | unwrapErr (Err.Primary {errorAt, repair = R.Substitution t}) = 
          (WStream.getPos errorAt, 
  	   Subst {
  	     old = [(#1 (WStream.get1 errorAt))],
  	     new = [t]
           })
      | unwrapErr (Err.Secondary {deleteFrom, deleteTo}) = 
          (WStream.getPos deleteFrom, 
  	   Delete (map #1 (WStream.getDiff (deleteTo, deleteFrom))))

    fun toksToString toks = String.concatWith " " (map Tok.toString toks)

    fun repairToString repair = (case repair
          of Insert toks => "inserting " ^ toksToString toks
	   | Delete toks => "deleting " ^ toksToString toks
	   | Subst {old, new} => 
	       "substituting " ^ toksToString new ^ " for "
	       ^ toksToString old
         (* end case *))

    fun mk () = let
        val eh = Err.mkErrHandler()
	fun wrap f = Err.wrap eh f
	val whileDisabled = Err.whileDisabled eh
	val addAnnotation = Err.addAnnotation eh
	fun tryProds (strm, prods) = 
	      (wrap (pretryProds eh prods)) strm
	fun unwrap (ret, strm, errors, anns) = (ret, WStream.unwrap strm, map unwrapErr errors, anns)
	val lex = WStream.get1
val matchEOF = wrap (fn strm => (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDummyExp = wrap (fn strm => (case (lex(strm))
 of (Tok.DummyExp(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSEMI = wrap (fn strm => (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchRP = wrap (fn strm => (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchLP = wrap (fn strm => (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchMINUS = wrap (fn strm => (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchTIMES = wrap (fn strm => (case (lex(strm))
 of (Tok.TIMES, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchPLUS = wrap (fn strm => (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchEQ = wrap (fn strm => (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchNUM = wrap (fn strm => (case (lex(strm))
 of (Tok.NUM(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchID = wrap (fn strm => (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_in = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_in, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_let = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_let, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))

val (prog_NT, exp_NT) = 
let
fun exp_NT (env_RES) (strm) = let
      fun exp_PROD_1 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (exp1_RES, exp1_SPAN, strm') = (wrap (exp_NT (UserCode.ARGS_3 (EQ_RES, ID_RES, env_RES, KW_let_RES))))(strm')
            val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
            val (exp2_RES, exp2_SPAN, strm') = (wrap (exp_NT (UserCode.ARGS_4 (EQ_RES, ID_RES, env_RES, exp1_RES, KW_in_RES, KW_let_RES))))(strm')
            val (EOF_RES, EOF_SPAN, strm') = matchEOF(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(EOF_SPAN))
            in
              (UserCode.exp_PROD_1_ACT (EQ_RES, ID_RES, EOF_RES, env_RES, exp1_RES, exp2_RES, KW_in_RES, KW_let_RES, EQ_SPAN : Lex.span, ID_SPAN : Lex.span, EOF_SPAN : Lex.span, exp1_SPAN : Lex.span, exp2_SPAN : Lex.span, KW_in_SPAN : Lex.span, KW_let_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun exp_PROD_2 (strm) = let
            val (addExp_RES, addExp_SPAN, strm') = (wrap (addExp_NT (UserCode.ARGS_5 (env_RES))))(strm)
            val (EOF_RES, EOF_SPAN, strm') = matchEOF(strm')
            val FULL_SPAN = (#1(addExp_SPAN), #2(EOF_SPAN))
            in
              (addExp_RES, FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.DummyExp(_), _, strm') => exp_PROD_2(strm)
          | (Tok.LP, _, strm') => exp_PROD_2(strm)
          | (Tok.MINUS, _, strm') => exp_PROD_2(strm)
          | (Tok.NUM(_), _, strm') => exp_PROD_2(strm)
          | (Tok.ID(_), _, strm') => exp_PROD_2(strm)
          | (Tok.KW_let, _, strm') => exp_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
and addExp_NT (env_RES) (strm) = let
      val (multExp_RES, multExp_SPAN, strm') = (wrap (multExp_NT (UserCode.ARGS_7 (env_RES))))(strm)
      fun SR1_NT (strm) = let
            val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
            val (multExp_RES, multExp_SPAN, strm') = (wrap (multExp_NT (UserCode.ARGS_8 (env_RES, PLUS_RES, multExp_RES))))(strm')
            val FULL_SPAN = (#1(PLUS_SPAN), #2(multExp_SPAN))
            in
              (multExp_RES, FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.PLUS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      val FULL_SPAN = (#1(multExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.addExp_PROD_1_ACT (SR_RES, env_RES, multExp_RES, SR_SPAN : Lex.span, multExp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
and multExp_NT (env_RES) (strm) = let
      val (prefixExp_RES, prefixExp_SPAN, strm') = (wrap (prefixExp_NT (UserCode.ARGS_10 (env_RES))))(strm)
      fun SR1_NT (strm) = let
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = (wrap (prefixExp_NT (UserCode.ARGS_11 (env_RES, TIMES_RES, prefixExp_RES))))(strm')
            val FULL_SPAN = (#1(TIMES_SPAN), #2(prefixExp_SPAN))
            in
              (prefixExp_RES, FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.TIMES, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      val FULL_SPAN = (#1(prefixExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.multExp_PROD_1_ACT (SR_RES, env_RES, prefixExp_RES, SR_SPAN : Lex.span, prefixExp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
and prefixExp_NT (env_RES) (strm) = let
      fun prefixExp_PROD_1 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = (wrap (atomicExp_NT (UserCode.ARGS_12 (env_RES))))(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              (atomicExp_RES, FULL_SPAN, strm')
            end
      fun prefixExp_PROD_2 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = (wrap (prefixExp_NT (UserCode.ARGS_14 (env_RES, MINUS_RES))))(strm')
            val FULL_SPAN = (#1(MINUS_SPAN), #2(prefixExp_SPAN))
            in
              (UserCode.prefixExp_PROD_2_ACT (env_RES, MINUS_RES, prefixExp_RES, MINUS_SPAN : Lex.span, prefixExp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.MINUS, _, strm') => prefixExp_PROD_2(strm)
          | (Tok.DummyExp(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.LP, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.NUM(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.ID(_), _, strm') => prefixExp_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
and atomicExp_NT (env_RES) (strm) = let
      fun atomicExp_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            in
              if (UserCode.atomicExp_PROD_1_PRED (ID_RES, env_RES))
                then let
                  val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
                  in
                    (UserCode.atomicExp_PROD_1_ACT (ID_RES, env_RES, ID_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                      FULL_SPAN, strm')
                  end
                else raise ParseError
            end
      fun atomicExp_PROD_2 (strm) = let
            val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
            val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
            in
              (NUM_RES, FULL_SPAN, strm')
            end
      fun atomicExp_PROD_3 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_RES, exp_SPAN, strm') = (wrap (exp_NT (UserCode.ARGS_17 (LP_RES, env_RES))))(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (exp_RES, FULL_SPAN, strm')
            end
      fun atomicExp_PROD_4 (strm) = let
            val (DummyExp_RES, DummyExp_SPAN, strm') = matchDummyExp(strm)
            val FULL_SPAN = (#1(DummyExp_SPAN), #2(DummyExp_SPAN))
            in
              (DummyExp_RES, FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.DummyExp(_), _, strm') => atomicExp_PROD_4(strm)
          | (Tok.NUM(_), _, strm') => atomicExp_PROD_2(strm)
          | (Tok.ID(_), _, strm') => atomicExp_PROD_1(strm)
          | (Tok.LP, _, strm') => atomicExp_PROD_3(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun prog_NT (strm) = let
      fun SR1_NT (strm) = let
            val (exp_RES, exp_SPAN, strm') = (wrap (exp_NT (UserCode.ARGS_1 ())))(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(exp_SPAN), #2(SEMI_SPAN))
            in
              (exp_RES, FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.DummyExp(_), _, strm') => true
              | (Tok.LP, _, strm') => true
              | (Tok.MINUS, _, strm') => true
              | (Tok.NUM(_), _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | (Tok.KW_let, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm)
      val (EOF_RES, EOF_SPAN, strm') = matchEOF(strm')
      val FULL_SPAN = (#1(SR_SPAN), #2(EOF_SPAN))
      in
        (SR_RES, FULL_SPAN, strm')
      end
in
  (prog_NT, exp_NT)
end
val prog_NT =  fn s => unwrap (Err.launch eh (prog_NT ) (WStream.wrap s))
val exp_NT =  fn x => fn s => unwrap (Err.launch eh (exp_NT x ) (WStream.wrap s))

in (prog_NT, exp_NT) end

fun parse s = let val (prog_NT, exp_NT) = mk() in prog_NT s end

fun parseexp x s = let val (prog_NT, exp_NT) = mk() in exp_NT x s end


end (* structure Parser *)
