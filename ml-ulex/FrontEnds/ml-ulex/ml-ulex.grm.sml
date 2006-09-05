structure Tok = struct

    datatype token = EOF
      | BOGUS
      | CODE of string
      | ID of string
      | CHAR of char
      | STRING of string
      | ASCII8
      | ASCII7
      | UTF8
      | KW_charset
      | KW_let
      | KW_states
      | KW_name
      | KW_defs
      | EQ
      | DARROW
      | DASH
      | CARAT
      | COMMA
      | SLASH
      | GT
      | LT
      | RCB
      | LCB
      | RSB
      | LSB
      | RP
      | LP
      | SEMI
      | QUERY
      | STAR
      | PLUS
      | DOLLAR
      | DOT
      | AMP
      | BAR

    fun toString tok = 
(case (tok)
 of (EOF) => "EOF"
  | (BOGUS) => "BOGUS"
  | (CODE(_)) => "CODE"
  | (ID(_)) => "ID"
  | (CHAR(_)) => "CHAR"
  | (STRING(_)) => "STRING"
  | (ASCII8) => "ASCII8"
  | (ASCII7) => "ASCII7"
  | (UTF8) => "UTF8"
  | (KW_charset) => "KW_charset"
  | (KW_let) => "KW_let"
  | (KW_states) => "KW_states"
  | (KW_name) => "KW_name"
  | (KW_defs) => "KW_defs"
  | (EQ) => "EQ"
  | (DARROW) => "DARROW"
  | (DASH) => "DASH"
  | (CARAT) => "CARAT"
  | (COMMA) => "COMMA"
  | (SLASH) => "SLASH"
  | (GT) => "GT"
  | (LT) => "LT"
  | (RCB) => "RCB"
  | (LCB) => "LCB"
  | (RSB) => "RSB"
  | (LSB) => "LSB"
  | (RP) => "RP"
  | (LP) => "LP"
  | (SEMI) => "SEMI"
  | (QUERY) => "QUERY"
  | (STAR) => "STAR"
  | (PLUS) => "PLUS"
  | (DOLLAR) => "DOLLAR"
  | (DOT) => "DOT"
  | (AMP) => "AMP"
  | (BAR) => "BAR"
(* end case *))


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

functor Parser(YY_Lex : LEXER)= struct

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
      fun unwrap (WSTREAM {strm, ...}) = strm

      fun get1 (WSTREAM {prefix = tok::toks, strm, curTok}) = 
	    (tok, WSTREAM {prefix = toks, strm = strm, curTok = curTok + 1})
	| get1 (WSTREAM {prefix = [], strm, curTok}) = let
	    val (tok, strm') = case YY_Lex.lex strm
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
      type T = YY_Lex.strm WS.wstream
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

    val allRepairs = [Deletion, Insertion Tok.EOF, Insertion Tok.BOGUS, Insertion Tok.ASCII8, Insertion Tok.ASCII7, Insertion Tok.UTF8, Insertion Tok.KW_charset, Insertion Tok.KW_let, Insertion Tok.KW_states, Insertion Tok.KW_name, Insertion Tok.KW_defs, Insertion Tok.EQ, Insertion Tok.DARROW, Insertion Tok.DASH, Insertion Tok.CARAT, Insertion Tok.COMMA, Insertion Tok.SLASH, Insertion Tok.GT, Insertion Tok.LT, Insertion Tok.RCB, Insertion Tok.LCB, Insertion Tok.RSB, Insertion Tok.LSB, Insertion Tok.RP, Insertion Tok.LP, Insertion Tok.SEMI, Insertion Tok.QUERY, Insertion Tok.STAR, Insertion Tok.PLUS, Insertion Tok.DOLLAR, Insertion Tok.DOT, Insertion Tok.AMP, Insertion Tok.BAR, Substitution Tok.EOF, Substitution Tok.BOGUS, Substitution Tok.ASCII8, Substitution Tok.ASCII7, Substitution Tok.UTF8, Substitution Tok.KW_charset, Substitution Tok.KW_let, Substitution Tok.KW_states, Substitution Tok.KW_name, Substitution Tok.KW_defs, Substitution Tok.EQ, Substitution Tok.DARROW, Substitution Tok.DASH, Substitution Tok.CARAT, Substitution Tok.COMMA, Substitution Tok.SLASH, Substitution Tok.GT, Substitution Tok.LT, Substitution Tok.RCB, Substitution Tok.LCB, Substitution Tok.RSB, Substitution Tok.LSB, Substitution Tok.RP, Substitution Tok.LP, Substitution Tok.SEMI, Substitution Tok.QUERY, Substitution Tok.STAR, Substitution Tok.PLUS, Substitution Tok.DOLLAR, Substitution Tok.DOT, Substitution Tok.AMP, Substitution Tok.BAR]


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

      fun isKW _ = true (* TODO *)

      fun involvesKW (r, t) = (case r
            of Insertion t' => isKW t'
	     | Deletion => isKW t
	     | Substitution t' => isKW t orelse isKW t'
           (* end case *))

      fun chooseCand (c1, c2) = let
	    val (r1, _, _, score1, kw1) = c1
	    val (r2, _, _, score2, kw2) = c2
	    fun chooseKWScore() = (case (kw1, kw2)
                  of (true, true) =>
		       if score1 > score2 then c1 else c2
		   | (false, false) => 
		       if score1 > score2 then c1 else c2
		   | (true, false) => c2
		   | (false, true) => c1
		 (* end case *))
            in case (r1, r2)
		of (Insertion _, Insertion _) => chooseKWScore()
		 | (Insertion _, _) => c1
		 | (_, Insertion _) => c2
		 | (Deletion, Deletion) => chooseKWScore()
		 | (Deletion, _) => c1
		 | (_, Deletion) => c2
		 | _ => chooseKWScore()
            end

      fun chooseRepair {startAt, endAt, try} = let
	    val (endAt', working) = getWorking 
				      (startAt, 
				       WS.subtract (endAt, startAt) + 5, [])
	    val scoreOffset = List.length working
	    fun tryRepairs (prefix, working, repairs, best) = (case (working, repairs)
	      of ([], _) => (case best
			      of SOME (r, prefixLen, strm, _, _) => 
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

      val wrap   : err_handler -> (R.T -> ('a * R.T)) -> R.T -> ('a * R.T)
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
	    val rightMost = hd stack
	    fun TOf (t, _) = t
	    fun find [] = raise (Fail "BUG: findWindow given an empty stack")
	      | find [top] = (top, top)
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

    fun tryProds eh prods strm = let
	fun try [] = raise RepairableStrm.RepairableError
	  | try (prod :: prods) = 
	      Err.whileDisabled eh (fn () => prod strm)
	      handle _ => try (prods)
        in
          try prods
        end

  end (* structure YY *)

 
  structure LS = LexSpec
  structure AMap = AtomMap

  structure S = LexSpec

  structure RE = RegExp
  structure SIS = RE.SymSet

  fun listToASet ls = AtomSet.addList (AtomSet.empty, ls)

  val wildcard = RE.mkSymSet (SIS.complement (SIS.singleton 0w10)) 
  fun charToSym c = Word32.fromInt (Char.ord c)

  fun flip (x, y) = (y, x)

  exception ParseError = YY.RepairableStrm.RepairableError

  fun innerParse
unwrapErr

strm = let
        val yyeh = YY.Err.mkErrHandler()
	fun yywrap f = YY.Err.wrap yyeh f
	val yylaunch = YY.Err.launch yyeh
	val yywhileDisabled = YY.Err.whileDisabled yyeh
	fun yytryProds (strm, prods) = 
	      (yywrap (YY.tryProds yyeh prods)) strm
	val yylex = yywrap YY.WStream.get1
fun yymatchEOF strm = (case (yylex(strm))
 of (Tok.EOF, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchBOGUS strm = (case (yylex(strm))
 of (Tok.BOGUS, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchCODE strm = (case (yylex(strm))
 of (Tok.CODE(x), strm') => (x, strm')
  | _ => raise(ParseError)
(* end case *))
fun yymatchID strm = (case (yylex(strm))
 of (Tok.ID(x), strm') => (x, strm')
  | _ => raise(ParseError)
(* end case *))
fun yymatchCHAR strm = (case (yylex(strm))
 of (Tok.CHAR(x), strm') => (x, strm')
  | _ => raise(ParseError)
(* end case *))
fun yymatchSTRING strm = (case (yylex(strm))
 of (Tok.STRING(x), strm') => (x, strm')
  | _ => raise(ParseError)
(* end case *))
fun yymatchASCII8 strm = (case (yylex(strm))
 of (Tok.ASCII8, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchASCII7 strm = (case (yylex(strm))
 of (Tok.ASCII7, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchUTF8 strm = (case (yylex(strm))
 of (Tok.UTF8, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchKW_charset strm = (case (yylex(strm))
 of (Tok.KW_charset, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchKW_let strm = (case (yylex(strm))
 of (Tok.KW_let, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchKW_states strm = (case (yylex(strm))
 of (Tok.KW_states, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchKW_name strm = (case (yylex(strm))
 of (Tok.KW_name, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchKW_defs strm = (case (yylex(strm))
 of (Tok.KW_defs, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchEQ strm = (case (yylex(strm))
 of (Tok.EQ, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchDARROW strm = (case (yylex(strm))
 of (Tok.DARROW, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchDASH strm = (case (yylex(strm))
 of (Tok.DASH, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchCARAT strm = (case (yylex(strm))
 of (Tok.CARAT, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchCOMMA strm = (case (yylex(strm))
 of (Tok.COMMA, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchSLASH strm = (case (yylex(strm))
 of (Tok.SLASH, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchGT strm = (case (yylex(strm))
 of (Tok.GT, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchLT strm = (case (yylex(strm))
 of (Tok.LT, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchRCB strm = (case (yylex(strm))
 of (Tok.RCB, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchLCB strm = (case (yylex(strm))
 of (Tok.LCB, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchRSB strm = (case (yylex(strm))
 of (Tok.RSB, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchLSB strm = (case (yylex(strm))
 of (Tok.LSB, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchRP strm = (case (yylex(strm))
 of (Tok.RP, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchLP strm = (case (yylex(strm))
 of (Tok.LP, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchSEMI strm = (case (yylex(strm))
 of (Tok.SEMI, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchQUERY strm = (case (yylex(strm))
 of (Tok.QUERY, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchSTAR strm = (case (yylex(strm))
 of (Tok.STAR, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchPLUS strm = (case (yylex(strm))
 of (Tok.PLUS, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchDOLLAR strm = (case (yylex(strm))
 of (Tok.DOLLAR, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchDOT strm = (case (yylex(strm))
 of (Tok.DOT, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchAMP strm = (case (yylex(strm))
 of (Tok.AMP, strm') => strm'
  | _ => raise(ParseError)
(* end case *))
fun yymatchBAR strm = (case (yylex(strm))
 of (Tok.BAR, strm') => strm'
  | _ => raise(ParseError)
(* end case *))

fun parse'  strm = 
let
fun char_NT (strm) = let
      val (CHAR, strm') = yymatchCHAR(strm)
      in
        (( charToSym CHAR), strm')
      end
fun re_NT (env) (strm) = let
      val (or_re, strm') = (yywrap (or_re_NT (env)))(strm)
      in
        ((or_re), strm')
      end
and or_re_NT (env) (strm) = let
      val (and_re, strm') = (yywrap (and_re_NT (env)))(strm)
      fun SR1_NT (strm) = let
            val strm' = yymatchBAR(strm)
            val (and_re, strm') = (yywrap (and_re_NT (env)))(strm')
            in
              ((and_re), strm')
            end
      fun SR1_PRED (strm) = (case (yylex(strm))
             of (Tok.BAR, strm') => true
              | (Tok.DARROW, strm') => false
              | (Tok.RP, strm') => false
              | (Tok.SEMI, strm') => false
              | _ => raise(ParseError)
            (* end case *))
      val (SR1, strm') = YY.EBNF.closure(SR1_PRED, (yywrap SR1_NT), strm')
      in
        (( foldl (RE.mkOr o flip) and_re SR1), strm')
      end
and and_re_NT (env) (strm) = let
      val (not_re, strm') = (yywrap (not_re_NT (env)))(strm)
      fun SR1_NT (strm) = let
            val strm' = yymatchAMP(strm)
            val (not_re, strm') = (yywrap (not_re_NT (env)))(strm')
            in
              ((not_re), strm')
            end
      fun SR1_PRED (strm) = (case (yylex(strm))
             of (Tok.AMP, strm') => true
              | (Tok.DARROW, strm') => false
              | (Tok.RP, strm') => false
              | (Tok.SEMI, strm') => false
              | (Tok.BAR, strm') => false
              | _ => raise(ParseError)
            (* end case *))
      val (SR1, strm') = YY.EBNF.closure(SR1_PRED, (yywrap SR1_NT), strm')
      in
        (( foldl (RE.mkAnd o flip) not_re SR1), strm')
      end
and not_re_NT (env) (strm) = let
      fun not_re_PROD_1 (strm) = let
            val strm' = yymatchCARAT(strm)
            val (cat_re, strm') = (yywrap (cat_re_NT (env)))(strm')
            in
              (( RE.mkNot cat_re), strm')
            end
      fun not_re_PROD_2 (strm) = let
            val (cat_re, strm') = (yywrap (cat_re_NT (env)))(strm)
            in
              ((cat_re), strm')
            end
      in
        (case (yylex(strm))
         of (Tok.ID(_), strm') => not_re_PROD_2(strm)
          | (Tok.CHAR(_), strm') => not_re_PROD_2(strm)
          | (Tok.STRING(_), strm') => not_re_PROD_2(strm)
          | (Tok.LSB, strm') => not_re_PROD_2(strm)
          | (Tok.LP, strm') => not_re_PROD_2(strm)
          | (Tok.DOT, strm') => not_re_PROD_2(strm)
          | (Tok.CARAT, strm') => not_re_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
and cat_re_NT (env) (strm) = let
      val (post_re, strm') = (yywrap (post_re_NT (env)))(strm)
      fun SR1_NT (strm) = let
            val (post_re, strm') = (yywrap (post_re_NT (env)))(strm)
            in
              ((post_re), strm')
            end
      fun SR1_PRED (strm) = (case (yylex(strm))
             of (Tok.ID(_), strm') => true
              | (Tok.CHAR(_), strm') => true
              | (Tok.STRING(_), strm') => true
              | (Tok.LSB, strm') => true
              | (Tok.LP, strm') => true
              | (Tok.DOT, strm') => true
              | (Tok.DARROW, strm') => false
              | (Tok.RP, strm') => false
              | (Tok.SEMI, strm') => false
              | (Tok.AMP, strm') => false
              | (Tok.BAR, strm') => false
              | _ => raise(ParseError)
            (* end case *))
      val (SR1, strm') = YY.EBNF.closure(SR1_PRED, (yywrap SR1_NT), strm')
      in
        (( foldl (RE.mkConcat o flip) post_re SR1), strm')
      end
and post_re_NT (env) (strm) = let
      val (prim_re, strm') = (yywrap (prim_re_NT (env)))(strm)
      val (SR1, strm') = let
      fun SR1_NT (strm) = let
            fun SR1_PROD_1 (strm) = let
                  val strm' = yymatchQUERY(strm)
                  in
                    (( RE.mkOpt), strm')
                  end
            fun SR1_PROD_2 (strm) = let
                  val strm' = yymatchSTAR(strm)
                  in
                    (( RE.mkClosure), strm')
                  end
            fun SR1_PROD_3 (strm) = let
                  val strm' = yymatchPLUS(strm)
                  in
                    (( fn re => RE.mkAtLeast (re, 1)), strm')
                  end
            fun SR1_PROD_4 (strm) = (( fn x => x), strm)
            in
              (case (yylex(strm))
               of (Tok.ID(_), strm') => SR1_PROD_4(strm)
                | (Tok.CHAR(_), strm') => SR1_PROD_4(strm)
                | (Tok.STRING(_), strm') => SR1_PROD_4(strm)
                | (Tok.DARROW, strm') => SR1_PROD_4(strm)
                | (Tok.LSB, strm') => SR1_PROD_4(strm)
                | (Tok.RP, strm') => SR1_PROD_4(strm)
                | (Tok.LP, strm') => SR1_PROD_4(strm)
                | (Tok.SEMI, strm') => SR1_PROD_4(strm)
                | (Tok.DOT, strm') => SR1_PROD_4(strm)
                | (Tok.AMP, strm') => SR1_PROD_4(strm)
                | (Tok.BAR, strm') => SR1_PROD_4(strm)
                | (Tok.STAR, strm') => SR1_PROD_2(strm)
                | (Tok.QUERY, strm') => SR1_PROD_1(strm)
                | (Tok.PLUS, strm') => SR1_PROD_3(strm)
                | _ => raise(ParseError)
              (* end case *))
            end
      in
        (yywrap SR1_NT)(strm')
      end
      in
        (( SR1 prim_re), strm')
      end
and prim_re_NT (env) (strm) = let
      fun prim_re_PROD_1 (strm) = let
            val (ID, strm') = yymatchID(strm)
            in
              (( valOf (AMap.find (env, Atom.atom ID))), strm')
            end
      fun prim_re_PROD_2 (strm) = let
            val (STRING, strm') = yymatchSTRING(strm)
            in
              ((  foldr RE.mkConcat
		 RE.epsilon 
		 (map (RE.mkSym o charToSym) (String.explode STRING)) ),
                strm')
            end
      fun prim_re_PROD_3 (strm) = let
            val strm' = yymatchLP(strm)
            val (re, strm') = (yywrap (re_NT (env)))(strm')
            val strm' = yymatchRP(strm')
            in
              ((re), strm')
            end
      fun prim_re_PROD_4 (strm) = let
            val (char, strm') = (yywrap char_NT)(strm)
            in
              (( RE.mkSym char), strm')
            end
      fun prim_re_PROD_5 (strm) = let
            val strm' = yymatchDOT(strm)
            in
              (( wildcard), strm')
            end
      fun prim_re_PROD_6 (strm) = let
            val strm' = yymatchLSB(strm)
            val (SR1, strm') = let
            fun SR1_NT (strm) = let
                  fun SR1_PROD_1 (strm) = let
                        val strm' = yymatchCARAT(strm)
                        in
                          (( SIS.complement), strm')
                        end
                  fun SR1_PROD_2 (strm) = (( fn x => x), strm)
                  in
                    (case (yylex(strm))
                     of (Tok.CHAR(_), strm') => SR1_PROD_2(strm)
                      | (Tok.CARAT, strm') => SR1_PROD_1(strm)
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            in
              (yywrap SR1_NT)(strm')
            end
            fun SR2_NT (strm) = let
                  fun SR2_PROD_1 (strm) = let
                        val (char1, strm') = (yywrap char_NT)(strm)
                        val strm' = yymatchDASH(strm')
                        val (char2, strm') = (yywrap char_NT)(strm')
                        in
                          (( SIS.interval (char1, char2)), strm')
                        end
                  fun SR2_PROD_2 (strm) = let
                        val (char, strm') = (yywrap char_NT)(strm)
                        in
                          (( SIS.singleton char), strm')
                        end
                  in
                    (case (yylex(strm))
                     of (Tok.CHAR(_), strm') =>
                          (case (yylex(strm'))
                           of (Tok.DASH, strm') => SR2_PROD_1(strm)
                            | (Tok.CHAR(_), strm') => SR2_PROD_2(strm)
                            | (Tok.RSB, strm') => SR2_PROD_2(strm)
                            | _ => raise(ParseError)
                          (* end case *))
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            fun SR2_PRED (strm) = (case (yylex(strm))
                   of (Tok.CHAR(_), strm') => true
                    | (Tok.RSB, strm') => false
                    | _ => raise(ParseError)
                  (* end case *))
            val (SR2, strm') = YY.EBNF.posclos(SR2_PRED, (yywrap SR2_NT), strm')
            val strm' = yymatchRSB(strm')
            in
              (( RE.mkSymSet (SR1 (foldl SIS.union (hd SR2) (tl SR2)))), strm')
            end
      in
        (case (yylex(strm))
         of (Tok.LSB, strm') => prim_re_PROD_6(strm)
          | (Tok.CHAR(_), strm') => prim_re_PROD_4(strm)
          | (Tok.STRING(_), strm') => prim_re_PROD_2(strm)
          | (Tok.ID(_), strm') => prim_re_PROD_1(strm)
          | (Tok.LP, strm') => prim_re_PROD_3(strm)
          | (Tok.DOT, strm') => prim_re_PROD_5(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun rule_NT (env) (strm) = let
      fun SR1_NT (strm) = let
            val strm' = yymatchLT(strm)
            fun SR1_NT (strm) = let
                  val (ID, strm') = yymatchID(strm)
                  fun SR1_NT (strm) = let
                        val strm' = yymatchCOMMA(strm)
                        in
                          ((), strm')
                        end
                  fun SR1_PRED (strm) = (case (yylex(strm))
                         of (Tok.COMMA, strm') => true
                          | (Tok.ID(_), strm') => false
                          | (Tok.GT, strm') => false
                          | _ => raise(ParseError)
                        (* end case *))
                  val (SR1, strm') = YY.EBNF.optional(SR1_PRED, (yywrap SR1_NT), strm')
                  in
                    (( ID), strm')
                  end
            fun SR1_PRED (strm) = (case (yylex(strm))
                   of (Tok.ID(_), strm') => true
                    | (Tok.GT, strm') => false
                    | _ => raise(ParseError)
                  (* end case *))
            val (SR1, strm') = YY.EBNF.posclos(SR1_PRED, (yywrap SR1_NT), strm')
            val strm' = yymatchGT(strm')
            in
              ((SR1), strm')
            end
      fun SR1_PRED (strm) = (case (yylex(strm))
             of (Tok.LT, strm') => true
              | (Tok.ID(_), strm') => false
              | (Tok.CHAR(_), strm') => false
              | (Tok.STRING(_), strm') => false
              | (Tok.CARAT, strm') => false
              | (Tok.LSB, strm') => false
              | (Tok.LP, strm') => false
              | (Tok.DOT, strm') => false
              | _ => raise(ParseError)
            (* end case *))
      val (SR1, strm') = YY.EBNF.optional(SR1_PRED, (yywrap SR1_NT), strm)
      val (re, strm') = (yywrap (re_NT (env)))(strm')
      val strm' = yymatchDARROW(strm')
      val (CODE, strm') = yymatchCODE(strm')
      in
        (( (Option.map (listToASet o (map Atom.atom)) SR1, re), CODE), strm')
      end
fun directive_NT (conf, env) (strm) = let
      fun directive_PROD_1 (strm) = let
            val strm' = yymatchKW_let(strm)
            val (ID, strm') = yymatchID(strm')
            val strm' = yymatchEQ(strm')
            val (re, strm') = (yywrap (re_NT (env)))(strm')
            in
              (( conf, AMap.insert (env, Atom.atom ID, re)), strm')
            end
      fun directive_PROD_2 (strm) = let
            val strm' = yymatchKW_states(strm)
            fun SR1_NT (strm) = let
                  val (ID, strm') = yymatchID(strm)
                  fun SR1_NT (strm) = let
                        val strm' = yymatchCOMMA(strm)
                        in
                          ((), strm')
                        end
                  fun SR1_PRED (strm) = (case (yylex(strm))
                         of (Tok.COMMA, strm') => true
                          | (Tok.ID(_), strm') => false
                          | (Tok.SEMI, strm') => false
                          | _ => raise(ParseError)
                        (* end case *))
                  val (SR1, strm') = YY.EBNF.optional(SR1_PRED, (yywrap SR1_NT), strm')
                  in
                    (( ID), strm')
                  end
            fun SR1_PRED (strm) = (case (yylex(strm))
                   of (Tok.ID(_), strm') => true
                    | (Tok.SEMI, strm') => false
                    | _ => raise(ParseError)
                  (* end case *))
            val (SR1, strm') = YY.EBNF.posclos(SR1_PRED, (yywrap SR1_NT), strm')
            in
              (( LS.updStartStates (conf, listToASet (map Atom.atom SR1)), 
	  env),
                strm')
            end
      fun directive_PROD_3 (strm) = let
            val strm' = yymatchKW_charset(strm)
            val (SR1, strm') = let
            fun SR1_NT (strm) = let
                  fun SR1_PROD_1 (strm) = let
                        val strm' = yymatchUTF8(strm)
                        in
                          (( LS.updClamp (conf, LS.NO_CLAMP), env), strm')
                        end
                  fun SR1_PROD_2 (strm) = let
                        val strm' = yymatchASCII7(strm)
                        in
                          ((  LS.updClamp (conf, LS.CLAMP127), env), strm')
                        end
                  fun SR1_PROD_3 (strm) = let
                        val strm' = yymatchASCII8(strm)
                        in
                          ((  LS.updClamp (conf, LS.CLAMP255), env), strm')
                        end
                  in
                    (case (yylex(strm))
                     of (Tok.ASCII8, strm') => SR1_PROD_3(strm)
                      | (Tok.UTF8, strm') => SR1_PROD_1(strm)
                      | (Tok.ASCII7, strm') => SR1_PROD_2(strm)
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            in
              (yywrap SR1_NT)(strm')
            end
            in
              ((SR1), strm')
            end
      fun directive_PROD_4 (strm) = let
            val strm' = yymatchKW_name(strm)
            val (ID, strm') = yymatchID(strm')
            in
              (( LS.updStructName (conf, ID), env), strm')
            end
      in
        (case (yylex(strm))
         of (Tok.KW_name, strm') => directive_PROD_4(strm)
          | (Tok.KW_states, strm') => directive_PROD_2(strm)
          | (Tok.KW_let, strm') => directive_PROD_1(strm)
          | (Tok.KW_charset, strm') => directive_PROD_3(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun decl_NT (spec, env) (strm) = let
      fun decl_PROD_1 (strm) = let
            val (directive, strm') = (yywrap (directive_NT (LS.getConf spec, env)))(strm)
            in
              (( let val (conf', env') = directive
	  in 
	    (LS.updConf (spec, conf'),
	     env')
	  end),
                strm')
            end
      fun decl_PROD_2 (strm) = let
            val strm' = yymatchKW_defs(strm)
            val (CODE, strm') = yymatchCODE(strm')
            in
              (( LS.updDecls (spec, CODE), env), strm')
            end
      fun decl_PROD_3 (strm) = let
            val (rule, strm') = (yywrap (rule_NT (env)))(strm)
            in
              (( LS.addRule (spec, rule), env), strm')
            end
      in
        (case (yylex(strm))
         of (Tok.ID(_), strm') => decl_PROD_3(strm)
          | (Tok.CHAR(_), strm') => decl_PROD_3(strm)
          | (Tok.STRING(_), strm') => decl_PROD_3(strm)
          | (Tok.CARAT, strm') => decl_PROD_3(strm)
          | (Tok.LT, strm') => decl_PROD_3(strm)
          | (Tok.LSB, strm') => decl_PROD_3(strm)
          | (Tok.LP, strm') => decl_PROD_3(strm)
          | (Tok.DOT, strm') => decl_PROD_3(strm)
          | (Tok.KW_charset, strm') => decl_PROD_1(strm)
          | (Tok.KW_let, strm') => decl_PROD_1(strm)
          | (Tok.KW_states, strm') => decl_PROD_1(strm)
          | (Tok.KW_name, strm') => decl_PROD_1(strm)
          | (Tok.KW_defs, strm') => decl_PROD_2(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun decls_NT (spec, env) (strm) = let
      fun decls_PROD_1 (strm) = let
            val (decl, strm') = (yywrap (decl_NT (spec, env)))(strm)
            val strm' = yymatchSEMI(strm')
            val (decls, strm') = (yywrap (decls_NT (decl)))(strm')
            in
              (( decls), strm')
            end
      fun decls_PROD_2 (strm) = (( spec), strm)
      in
        (case (yylex(strm))
         of (Tok.EOF, strm') => decls_PROD_2(strm)
          | (Tok.ID(_), strm') => decls_PROD_1(strm)
          | (Tok.CHAR(_), strm') => decls_PROD_1(strm)
          | (Tok.STRING(_), strm') => decls_PROD_1(strm)
          | (Tok.KW_charset, strm') => decls_PROD_1(strm)
          | (Tok.KW_let, strm') => decls_PROD_1(strm)
          | (Tok.KW_states, strm') => decls_PROD_1(strm)
          | (Tok.KW_name, strm') => decls_PROD_1(strm)
          | (Tok.KW_defs, strm') => decls_PROD_1(strm)
          | (Tok.CARAT, strm') => decls_PROD_1(strm)
          | (Tok.LT, strm') => decls_PROD_1(strm)
          | (Tok.LSB, strm') => decls_PROD_1(strm)
          | (Tok.LP, strm') => decls_PROD_1(strm)
          | (Tok.DOT, strm') => decls_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun file_NT (strm) = let
      val (decls, strm') = (yywrap (decls_NT (LS.mkSpec(), AMap.empty)))(strm)
      val strm' = yymatchEOF(strm')
      in
        ((decls), strm')
      end
in
  file_NT(strm)
end
        val (ret, _, errors) = yylaunch (parse'

) (YY.WStream.wrap strm)
        in 
          (ret, map unwrapErr errors)
        end

  datatype repair_action
    = Insert of Tok.token list
    | Delete of Tok.token list
    | Subst of {
	old : Tok.token list, 
	new : Tok.token list
      }

  structure Err = YY.Err
  structure R = YY.RepairableStrm

  fun unwrapErr (Err.Primary {errorAt, repair = R.Deletion}) =
        (YY.WStream.unwrap errorAt, Delete [(#1 (YY.WStream.get1 errorAt))])
    | unwrapErr (Err.Primary {errorAt, repair = R.Insertion t}) =
        (YY.WStream.unwrap errorAt, Insert [t])
    | unwrapErr (Err.Primary {errorAt, repair = R.Substitution t}) = 
        (YY.WStream.unwrap errorAt, 
	 Subst {
	   old = [(#1 (YY.WStream.get1 errorAt))],
	   new = [t]
         })
    | unwrapErr (Err.Secondary {deleteFrom, deleteTo}) = 
        (YY.WStream.unwrap deleteFrom, 
	 Delete (YY.WStream.getDiff (deleteTo, deleteFrom)))

  val parse = innerParse unwrapErr

  fun toksToString toks = String.concatWith " " (map Tok.toString toks)

  fun repairToString repair = (case repair
        of Insert toks => "inserting " ^ toksToString toks
	 | Delete toks => "deleting " ^ toksToString toks
	 | Subst {old, new} => 
	     "substituting " ^ toksToString old ^ " for "
	     ^ toksToString new
       (* end case *))


(*
	  fun getDiff (strm, strm', accum) =
	        if WS.subtract (strm, strm') = 0 
		then rev accum
		else let
		  val (tok, strm'') = WS.get1 strm'
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
			     if WS.subtract (strm', strm) >= minAdvance + 2
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
		      val strm' = WS.prepend (prefix, strm)
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

(*    fun parser strm = parser' (WS.wrap strm) *)

end (* structure Parser *)
