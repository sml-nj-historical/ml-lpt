structure Tok = struct

    datatype token = EOF
      | BOGUS
      | CODE of string
      | ID of string
      | UCHAR of Word32.word
      | CHAR of char
      | REPEAT of int
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
  | (UCHAR(_)) => "UCHAR"
  | (CHAR(_)) => "CHAR"
  | (REPEAT(_)) => "REPEAT"
  | (ASCII8) => "ascii8"
  | (ASCII7) => "ascii7"
  | (UTF8) => "utf8"
  | (KW_charset) => "%charset"
  | (KW_let) => "%let"
  | (KW_states) => "%states"
  | (KW_name) => "%name"
  | (KW_defs) => "%defs"
  | (EQ) => "="
  | (DARROW) => "=>"
  | (DASH) => "-"
  | (CARAT) => "^"
  | (COMMA) => ","
  | (SLASH) => "/"
  | (GT) => ">"
  | (LT) => "<"
  | (RCB) => "}"
  | (LCB) => "{"
  | (RSB) => "]"
  | (LSB) => "["
  | (RP) => ")"
  | (LP) => "("
  | (SEMI) => ";"
  | (QUERY) => "?"
  | (STAR) => "*"
  | (PLUS) => "+"
  | (DOLLAR) => "$"
  | (DOT) => "."
  | (AMP) => "&"
  | (BAR) => "|"
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

functor Parser(Lex : LEXER)(* : sig

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

 
  structure LS = LexSpec
  structure AMap = AtomMap

  structure S = LexSpec

  structure RE = RegExp
  structure SIS = RE.SymSet

  fun listToASet ls = AtomSet.addList (AtomSet.empty, ls)

  val wildcard = RE.mkSymSet (SIS.complement (SIS.singleton 0w10)) 
  fun charToSym c = Word32.fromInt (Char.ord c)

  fun flip (x, y) = (y, x)


fun decls_PROD_1_ACT (env, SEMI, decl, spec, decls) = 
  ( decls)
fun decls_PROD_2_ACT (env, spec) = 
  ( spec)
fun decl_PROD_1_ACT (env, spec, directive) = 
  ( let val (conf', env') = directive
	  in 
	    (LS.updConf (spec, conf'),
	     env')
	  end)
fun decl_PROD_2_ACT (env, CODE, spec, KW_defs) = 
  ( LS.updDecls (spec, CODE), env)
fun decl_PROD_3_ACT (env, rule, spec) = 
  ( LS.addRule (spec, rule), env)
fun directive_PROD_1_ACT (EQ, ID, re, env, conf, KW_let) = 
  ( conf, AMap.insert (env, Atom.atom ID, re))
fun directive_PROD_2_directive_SR1_PROD_1_ACT (ID, env, conf, COMMA, KW_states) = 
  ( ID)
fun directive_PROD_2_ACT (SR, env, conf, KW_states) = 
  ( LS.updStartStates (conf, listToASet (map Atom.atom SR)), 
	  env)
fun directive_PROD_3_directive_SR1_PROD_1_ACT (env, UTF8, conf, KW_charset) = 
  ( LS.updClamp (conf, LS.NO_CLAMP), env)
fun directive_PROD_3_directive_SR1_PROD_2_ACT (env, conf, ASCII7, KW_charset) = 
  (  LS.updClamp (conf, LS.CLAMP127), env)
fun directive_PROD_3_directive_SR1_PROD_3_ACT (env, conf, ASCII8, KW_charset) = 
  (  LS.updClamp (conf, LS.CLAMP255), env)
fun directive_PROD_4_ACT (ID, env, conf, KW_name) = 
  ( LS.updStructName (conf, ID), env)
fun rule_PROD_1_rule_SR1_PROD_1_SR1_SR1_PROD_1_ACT (ID, LT, env, COMMA) = 
  ( ID)
fun rule_PROD_1_ACT (SR, re, env, CODE, DARROW) = 
  ( (Option.map (listToASet o (map Atom.atom)) SR, re), CODE)
fun or_re_PROD_1_ACT (SR, env, and_re) = 
  ( foldl (RE.mkOr o flip) and_re SR)
fun and_re_PROD_1_ACT (SR, env, not_re) = 
  ( foldl (RE.mkAnd o flip) not_re SR)
fun not_re_PROD_1_ACT (env, cat_re, CARAT) = 
  ( RE.mkNot cat_re)
fun cat_re_PROD_1_ACT (SR, env, post_re) = 
  ( foldl (RE.mkConcat o flip) post_re SR)
fun post_re_PROD_1_post_re_SR1_PROD_1_ACT (env, prim_re, QUERY) = 
  ( RE.mkOpt)
fun post_re_PROD_1_post_re_SR1_PROD_2_ACT (env, STAR, prim_re) = 
  ( RE.mkClosure)
fun post_re_PROD_1_post_re_SR1_PROD_3_ACT (env, PLUS, prim_re) = 
  ( fn re => RE.mkAtLeast (re, 1))
fun post_re_PROD_1_post_re_SR1_PROD_4_ACT (env, prim_re, REPEAT) = 
  ( fn re => RE.mkRep (re, REPEAT, REPEAT))
fun post_re_PROD_1_post_re_SR1_PROD_5_ACT (env, prim_re) = 
  ( fn x => x)
fun post_re_PROD_1_ACT (SR, env, prim_re) = 
  ( SR prim_re)
fun prim_re_PROD_1_ACT (ID, env) = 
  ( case (AMap.find (env, Atom.atom ID))
	   of SOME re => re
	    | NONE => (print (String.concat [
		"Error: ", ID, " is undefined.\n"]);
		RE.any))
fun prim_re_PROD_3_ACT (env, char) = 
  ( RE.mkSym char)
fun prim_re_PROD_4_ACT (DOT, env) = 
  ( wildcard)
fun prim_re_PROD_5_prim_re_SR1_PROD_1_ACT (LSB, env, CARAT) = 
  ( SIS.complement)
fun prim_re_PROD_5_prim_re_SR1_PROD_2_ACT (LSB, env) = 
  ( fn x => x)
fun prim_re_PROD_5_prim_re_SR2_PROD_1_ACT (LSB, SR1, env, DASH, char1, char2) = 
  ( 
	    if char1 <= char2 then
	       SIS.interval (char1, char2)
	     else (print (String.concat [
	       "Error: malformed character class: ",
	       Word32.toString char1, " - ",
	       Word32.toString char2, ".\n"]);
	       SIS.universe))
fun prim_re_PROD_5_prim_re_SR2_PROD_2_ACT (LSB, SR1, env, char) = 
  ( SIS.singleton char)
fun prim_re_PROD_5_ACT (LSB, RSB, SR1, SR2, env) = 
  ( RE.mkSymSet (SR1 (foldl SIS.union (hd SR2) (tl SR2))))
fun char_PROD_1_ACT (CHAR) = 
  ( charToSym CHAR)
fun ARGS_1 () = 
  (LS.mkSpec(), AMap.empty)
fun ARGS_3 (env, spec) = 
  (spec, env)
fun ARGS_4 (env, SEMI, decl, spec) = 
  (decl)
fun ARGS_7 (env, spec) = 
  (LS.getConf spec, env)
fun ARGS_10 (env, spec) = 
  (env)
fun ARGS_12 (EQ, ID, env, conf, KW_let) = 
  (env)
fun ARGS_21 (SR, env) = 
  (env)
fun ARGS_22 (env) = 
  (env)
fun ARGS_25 (BAR, env, and_re) = 
  (env)
fun ARGS_24 (env) = 
  (env)
fun ARGS_28 (AMP, env, not_re) = 
  (env)
fun ARGS_27 (env) = 
  (env)
fun ARGS_30 (env, CARAT) = 
  (env)
fun ARGS_31 (env) = 
  (env)
fun ARGS_34 (env, post_re) = 
  (env)
fun ARGS_33 (env) = 
  (env)
fun ARGS_36 (env) = 
  (env)
fun ARGS_43 (LP, env) = 
  (env)

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
(*	val launch = Err.launch eh *)
	val whileDisabled = Err.whileDisabled eh
	fun tryProds (strm, prods) = 
	      (wrap (pretryProds eh prods)) strm
(*
        fun wrapParse f x s = let
	      val (ret, strm', errors) = launch (f x) (WStream.wrap s)
	      in
	        (ret, strm', map unwrapErr errors)
	      end
        fun wrapParseNoArg f s = let
	      val (ret, strm', errors) = launch f (WStream.wrap s)
	      in
	        (ret, strm', map unwrapErr errors)
	      end
*)
	fun unwrap (ret, strm, errors) = (ret, strm, map unwrapErr errors)
	val lex = WStream.get1
val matchEOF = wrap (fn strm => (case (lex(strm))
 of (Tok.EOF, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchBOGUS = wrap (fn strm => (case (lex(strm))
 of (Tok.BOGUS, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCODE = wrap (fn strm => (case (lex(strm))
 of (Tok.CODE(x), strm') => (x, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchID = wrap (fn strm => (case (lex(strm))
 of (Tok.ID(x), strm') => (x, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchUCHAR = wrap (fn strm => (case (lex(strm))
 of (Tok.UCHAR(x), strm') => (x, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCHAR = wrap (fn strm => (case (lex(strm))
 of (Tok.CHAR(x), strm') => (x, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchREPEAT = wrap (fn strm => (case (lex(strm))
 of (Tok.REPEAT(x), strm') => (x, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchASCII8 = wrap (fn strm => (case (lex(strm))
 of (Tok.ASCII8, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchASCII7 = wrap (fn strm => (case (lex(strm))
 of (Tok.ASCII7, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchUTF8 = wrap (fn strm => (case (lex(strm))
 of (Tok.UTF8, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_charset = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_charset, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_let = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_let, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_states = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_states, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_name = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_name, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_defs = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_defs, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchEQ = wrap (fn strm => (case (lex(strm))
 of (Tok.EQ, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDARROW = wrap (fn strm => (case (lex(strm))
 of (Tok.DARROW, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDASH = wrap (fn strm => (case (lex(strm))
 of (Tok.DASH, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCARAT = wrap (fn strm => (case (lex(strm))
 of (Tok.CARAT, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCOMMA = wrap (fn strm => (case (lex(strm))
 of (Tok.COMMA, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSLASH = wrap (fn strm => (case (lex(strm))
 of (Tok.SLASH, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchGT = wrap (fn strm => (case (lex(strm))
 of (Tok.GT, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchLT = wrap (fn strm => (case (lex(strm))
 of (Tok.LT, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchRCB = wrap (fn strm => (case (lex(strm))
 of (Tok.RCB, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchLCB = wrap (fn strm => (case (lex(strm))
 of (Tok.LCB, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchRSB = wrap (fn strm => (case (lex(strm))
 of (Tok.RSB, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchLSB = wrap (fn strm => (case (lex(strm))
 of (Tok.LSB, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchRP = wrap (fn strm => (case (lex(strm))
 of (Tok.RP, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchLP = wrap (fn strm => (case (lex(strm))
 of (Tok.LP, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSEMI = wrap (fn strm => (case (lex(strm))
 of (Tok.SEMI, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchQUERY = wrap (fn strm => (case (lex(strm))
 of (Tok.QUERY, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSTAR = wrap (fn strm => (case (lex(strm))
 of (Tok.STAR, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchPLUS = wrap (fn strm => (case (lex(strm))
 of (Tok.PLUS, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDOLLAR = wrap (fn strm => (case (lex(strm))
 of (Tok.DOLLAR, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDOT = wrap (fn strm => (case (lex(strm))
 of (Tok.DOT, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchAMP = wrap (fn strm => (case (lex(strm))
 of (Tok.AMP, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))
val matchBAR = wrap (fn strm => (case (lex(strm))
 of (Tok.BAR, strm') => ((), strm')
  | _ => raise(ParseError)
(* end case *)))

val (file_NT) = 
let
fun char_NT (strm) = let
      fun char_PROD_1 (strm) = let
            val (CHAR_RES, strm') = matchCHAR(strm)
            in
              (UserCode.char_PROD_1_ACT (CHAR_RES), strm')
            end
      fun char_PROD_2 (strm) = let
            val (UCHAR_RES, strm') = matchUCHAR(strm)
            in
              (UCHAR_RES, strm')
            end
      in
        (case (lex(strm))
         of (Tok.UCHAR(_), strm') => char_PROD_2(strm)
          | (Tok.CHAR(_), strm') => char_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun re_NT (env_RES) (strm) = let
      val (or_re_RES, strm') = (wrap (or_re_NT (UserCode.ARGS_22 (env_RES))))(strm)
      in
        (or_re_RES, strm')
      end
and or_re_NT (env_RES) (strm) = let
      val (and_re_RES, strm') = (wrap (and_re_NT (UserCode.ARGS_24 (env_RES))))(strm)
      fun SR1_NT (strm) = let
            val (BAR_RES, strm') = matchBAR(strm)
            val (and_re_RES, strm') = (wrap (and_re_NT (UserCode.ARGS_25 (BAR_RES, env_RES, and_re_RES))))(strm')
            in
              (and_re_RES, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.BAR, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      in
        (UserCode.or_re_PROD_1_ACT (SR_RES, env_RES, and_re_RES), strm')
      end
and and_re_NT (env_RES) (strm) = let
      val (not_re_RES, strm') = (wrap (not_re_NT (UserCode.ARGS_27 (env_RES))))(strm)
      fun SR1_NT (strm) = let
            val (AMP_RES, strm') = matchAMP(strm)
            val (not_re_RES, strm') = (wrap (not_re_NT (UserCode.ARGS_28 (AMP_RES, env_RES, not_re_RES))))(strm')
            in
              (not_re_RES, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.AMP, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      in
        (UserCode.and_re_PROD_1_ACT (SR_RES, env_RES, not_re_RES), strm')
      end
and not_re_NT (env_RES) (strm) = let
      fun not_re_PROD_1 (strm) = let
            val (CARAT_RES, strm') = matchCARAT(strm)
            val (cat_re_RES, strm') = (wrap (cat_re_NT (UserCode.ARGS_30 (env_RES, CARAT_RES))))(strm')
            in
              (UserCode.not_re_PROD_1_ACT (env_RES, cat_re_RES, CARAT_RES),
                strm')
            end
      fun not_re_PROD_2 (strm) = let
            val (cat_re_RES, strm') = (wrap (cat_re_NT (UserCode.ARGS_31 (env_RES))))(strm)
            in
              (cat_re_RES, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), strm') => not_re_PROD_2(strm)
          | (Tok.UCHAR(_), strm') => not_re_PROD_2(strm)
          | (Tok.CHAR(_), strm') => not_re_PROD_2(strm)
          | (Tok.LSB, strm') => not_re_PROD_2(strm)
          | (Tok.LP, strm') => not_re_PROD_2(strm)
          | (Tok.DOT, strm') => not_re_PROD_2(strm)
          | (Tok.CARAT, strm') => not_re_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
and cat_re_NT (env_RES) (strm) = let
      val (post_re_RES, strm') = (wrap (post_re_NT (UserCode.ARGS_33 (env_RES))))(strm)
      fun SR1_NT (strm) = let
            val (post_re_RES, strm') = (wrap (post_re_NT (UserCode.ARGS_34 (env_RES, post_re_RES))))(strm)
            in
              (post_re_RES, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), strm') => true
              | (Tok.UCHAR(_), strm') => true
              | (Tok.CHAR(_), strm') => true
              | (Tok.LSB, strm') => true
              | (Tok.LP, strm') => true
              | (Tok.DOT, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      in
        (UserCode.cat_re_PROD_1_ACT (SR_RES, env_RES, post_re_RES), strm')
      end
and post_re_NT (env_RES) (strm) = let
      val (prim_re_RES, strm') = (wrap (prim_re_NT (UserCode.ARGS_36 (env_RES))))(strm)
      val (SR_RES, strm') = let
      fun SR1_NT (strm) = let
            fun SR1_PROD_1 (strm) = let
                  val (QUERY_RES, strm') = matchQUERY(strm)
                  in
                    (UserCode.post_re_PROD_1_post_re_SR1_PROD_1_ACT (env_RES, prim_re_RES, QUERY_RES),
                      strm')
                  end
            fun SR1_PROD_2 (strm) = let
                  val (STAR_RES, strm') = matchSTAR(strm)
                  in
                    (UserCode.post_re_PROD_1_post_re_SR1_PROD_2_ACT (env_RES, STAR_RES, prim_re_RES),
                      strm')
                  end
            fun SR1_PROD_3 (strm) = let
                  val (PLUS_RES, strm') = matchPLUS(strm)
                  in
                    (UserCode.post_re_PROD_1_post_re_SR1_PROD_3_ACT (env_RES, PLUS_RES, prim_re_RES),
                      strm')
                  end
            fun SR1_PROD_4 (strm) = let
                  val (REPEAT_RES, strm') = matchREPEAT(strm)
                  in
                    (UserCode.post_re_PROD_1_post_re_SR1_PROD_4_ACT (env_RES, prim_re_RES, REPEAT_RES),
                      strm')
                  end
            fun SR1_PROD_5 (strm) = (UserCode.post_re_PROD_1_post_re_SR1_PROD_5_ACT (env_RES, prim_re_RES),
                    strm)
            in
              (case (lex(strm))
               of (Tok.ID(_), strm') => SR1_PROD_5(strm)
                | (Tok.UCHAR(_), strm') => SR1_PROD_5(strm)
                | (Tok.CHAR(_), strm') => SR1_PROD_5(strm)
                | (Tok.DARROW, strm') => SR1_PROD_5(strm)
                | (Tok.LSB, strm') => SR1_PROD_5(strm)
                | (Tok.RP, strm') => SR1_PROD_5(strm)
                | (Tok.LP, strm') => SR1_PROD_5(strm)
                | (Tok.SEMI, strm') => SR1_PROD_5(strm)
                | (Tok.DOT, strm') => SR1_PROD_5(strm)
                | (Tok.AMP, strm') => SR1_PROD_5(strm)
                | (Tok.BAR, strm') => SR1_PROD_5(strm)
                | (Tok.PLUS, strm') => SR1_PROD_3(strm)
                | (Tok.QUERY, strm') => SR1_PROD_1(strm)
                | (Tok.STAR, strm') => SR1_PROD_2(strm)
                | (Tok.REPEAT(_), strm') => SR1_PROD_4(strm)
                | _ => raise(ParseError)
              (* end case *))
            end
      in
        (wrap SR1_NT)(strm')
      end
      in
        (UserCode.post_re_PROD_1_ACT (SR_RES, env_RES, prim_re_RES), strm')
      end
and prim_re_NT (env_RES) (strm) = let
      fun prim_re_PROD_1 (strm) = let
            val (ID_RES, strm') = matchID(strm)
            in
              (UserCode.prim_re_PROD_1_ACT (ID_RES, env_RES), strm')
            end
      fun prim_re_PROD_2 (strm) = let
            val (LP_RES, strm') = matchLP(strm)
            val (re_RES, strm') = (wrap (re_NT (UserCode.ARGS_43 (LP_RES, env_RES))))(strm')
            val (RP_RES, strm') = matchRP(strm')
            in
              (re_RES, strm')
            end
      fun prim_re_PROD_3 (strm) = let
            val (char_RES, strm') = (wrap char_NT)(strm)
            in
              (UserCode.prim_re_PROD_3_ACT (env_RES, char_RES), strm')
            end
      fun prim_re_PROD_4 (strm) = let
            val (DOT_RES, strm') = matchDOT(strm)
            in
              (UserCode.prim_re_PROD_4_ACT (DOT_RES, env_RES), strm')
            end
      fun prim_re_PROD_5 (strm) = let
            val (LSB_RES, strm') = matchLSB(strm)
            val (SR1_RES, strm') = let
            fun SR1_NT (strm) = let
                  fun SR1_PROD_1 (strm) = let
                        val (CARAT_RES, strm') = matchCARAT(strm)
                        in
                          (UserCode.prim_re_PROD_5_prim_re_SR1_PROD_1_ACT (LSB_RES, env_RES, CARAT_RES),
                            strm')
                        end
                  fun SR1_PROD_2 (strm) = (UserCode.prim_re_PROD_5_prim_re_SR1_PROD_2_ACT (LSB_RES, env_RES),
                          strm)
                  in
                    (case (lex(strm))
                     of (Tok.UCHAR(_), strm') => SR1_PROD_2(strm)
                      | (Tok.CHAR(_), strm') => SR1_PROD_2(strm)
                      | (Tok.CARAT, strm') => SR1_PROD_1(strm)
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            in
              (wrap SR1_NT)(strm')
            end
            fun SR2_NT (strm) = let
                  fun SR2_PROD_1 (strm) = let
                        val (char1_RES, strm') = (wrap char_NT)(strm)
                        val (DASH_RES, strm') = matchDASH(strm')
                        val (char2_RES, strm') = (wrap char_NT)(strm')
                        in
                          (UserCode.prim_re_PROD_5_prim_re_SR2_PROD_1_ACT (LSB_RES, SR1_RES, env_RES, DASH_RES, char1_RES, char2_RES),
                            strm')
                        end
                  fun SR2_PROD_2 (strm) = let
                        val (char_RES, strm') = (wrap char_NT)(strm)
                        in
                          (UserCode.prim_re_PROD_5_prim_re_SR2_PROD_2_ACT (LSB_RES, SR1_RES, env_RES, char_RES),
                            strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.UCHAR(_), strm') =>
                          (case (lex(strm'))
                           of (Tok.DASH, strm') => SR2_PROD_1(strm)
                            | (Tok.UCHAR(_), strm') => SR2_PROD_2(strm)
                            | (Tok.CHAR(_), strm') => SR2_PROD_2(strm)
                            | (Tok.RSB, strm') => SR2_PROD_2(strm)
                            | _ => raise(ParseError)
                          (* end case *))
                      | (Tok.CHAR(_), strm') =>
                          (case (lex(strm'))
                           of (Tok.DASH, strm') => SR2_PROD_1(strm)
                            | (Tok.UCHAR(_), strm') => SR2_PROD_2(strm)
                            | (Tok.CHAR(_), strm') => SR2_PROD_2(strm)
                            | (Tok.RSB, strm') => SR2_PROD_2(strm)
                            | _ => raise(ParseError)
                          (* end case *))
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            fun SR2_PRED (strm) = (case (lex(strm))
                   of (Tok.UCHAR(_), strm') => true
                    | (Tok.CHAR(_), strm') => true
                    | _ => false
                  (* end case *))
            val (SR2_RES, strm') = EBNF.posclos(SR2_PRED, (wrap SR2_NT), strm')
            val (RSB_RES, strm') = matchRSB(strm')
            in
              (UserCode.prim_re_PROD_5_ACT (LSB_RES, RSB_RES, SR1_RES, SR2_RES, env_RES),
                strm')
            end
      in
        (case (lex(strm))
         of (Tok.LSB, strm') => prim_re_PROD_5(strm)
          | (Tok.UCHAR(_), strm') => prim_re_PROD_3(strm)
          | (Tok.CHAR(_), strm') => prim_re_PROD_3(strm)
          | (Tok.ID(_), strm') => prim_re_PROD_1(strm)
          | (Tok.LP, strm') => prim_re_PROD_2(strm)
          | (Tok.DOT, strm') => prim_re_PROD_4(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun rule_NT (env_RES) (strm) = let
      fun SR1_NT (strm) = let
            val (LT_RES, strm') = matchLT(strm)
            fun SR1_NT (strm) = let
                  val (ID_RES, strm') = matchID(strm)
                  fun SR1_NT (strm) = let
                        val (COMMA_RES, strm') = matchCOMMA(strm)
                        in
                          ((), strm')
                        end
                  fun SR1_PRED (strm) = (case (lex(strm))
                         of (Tok.COMMA, strm') => true
                          | _ => false
                        (* end case *))
                  val (COMMA_RES, strm') = EBNF.optional(SR1_PRED, (wrap SR1_NT), strm')
                  in
                    (UserCode.rule_PROD_1_rule_SR1_PROD_1_SR1_SR1_PROD_1_ACT (ID_RES, LT_RES, env_RES, COMMA_RES),
                      strm')
                  end
            fun SR1_PRED (strm) = (case (lex(strm))
                   of (Tok.ID(_), strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, strm') = EBNF.posclos(SR1_PRED, (wrap SR1_NT), strm')
            val (GT_RES, strm') = matchGT(strm')
            in
              (SR_RES, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.LT, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, strm') = EBNF.optional(SR1_PRED, (wrap SR1_NT), strm)
      val (re_RES, strm') = (wrap (re_NT (UserCode.ARGS_21 (SR_RES, env_RES))))(strm')
      val (DARROW_RES, strm') = matchDARROW(strm')
      val (CODE_RES, strm') = matchCODE(strm')
      in
        (UserCode.rule_PROD_1_ACT (SR_RES, re_RES, env_RES, CODE_RES, DARROW_RES),
          strm')
      end
fun directive_NT (conf_RES, env_RES) (strm) = let
      fun directive_PROD_1 (strm) = let
            val (KW_let_RES, strm') = matchKW_let(strm)
            val (ID_RES, strm') = matchID(strm')
            val (EQ_RES, strm') = matchEQ(strm')
            val (re_RES, strm') = (wrap (re_NT (UserCode.ARGS_12 (EQ_RES, ID_RES, env_RES, conf_RES, KW_let_RES))))(strm')
            in
              (UserCode.directive_PROD_1_ACT (EQ_RES, ID_RES, re_RES, env_RES, conf_RES, KW_let_RES),
                strm')
            end
      fun directive_PROD_2 (strm) = let
            val (KW_states_RES, strm') = matchKW_states(strm)
            fun SR1_NT (strm) = let
                  val (ID_RES, strm') = matchID(strm)
                  fun SR1_NT (strm) = let
                        val (COMMA_RES, strm') = matchCOMMA(strm)
                        in
                          ((), strm')
                        end
                  fun SR1_PRED (strm) = (case (lex(strm))
                         of (Tok.COMMA, strm') => true
                          | _ => false
                        (* end case *))
                  val (COMMA_RES, strm') = EBNF.optional(SR1_PRED, (wrap SR1_NT), strm')
                  in
                    (UserCode.directive_PROD_2_directive_SR1_PROD_1_ACT (ID_RES, env_RES, conf_RES, COMMA_RES, KW_states_RES),
                      strm')
                  end
            fun SR1_PRED (strm) = (case (lex(strm))
                   of (Tok.ID(_), strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, strm') = EBNF.posclos(SR1_PRED, (wrap SR1_NT), strm')
            in
              (UserCode.directive_PROD_2_ACT (SR_RES, env_RES, conf_RES, KW_states_RES),
                strm')
            end
      fun directive_PROD_3 (strm) = let
            val (KW_charset_RES, strm') = matchKW_charset(strm)
            val (SR_RES, strm') = let
            fun SR1_NT (strm) = let
                  fun SR1_PROD_1 (strm) = let
                        val (UTF8_RES, strm') = matchUTF8(strm)
                        in
                          (UserCode.directive_PROD_3_directive_SR1_PROD_1_ACT (env_RES, UTF8_RES, conf_RES, KW_charset_RES),
                            strm')
                        end
                  fun SR1_PROD_2 (strm) = let
                        val (ASCII7_RES, strm') = matchASCII7(strm)
                        in
                          (UserCode.directive_PROD_3_directive_SR1_PROD_2_ACT (env_RES, conf_RES, ASCII7_RES, KW_charset_RES),
                            strm')
                        end
                  fun SR1_PROD_3 (strm) = let
                        val (ASCII8_RES, strm') = matchASCII8(strm)
                        in
                          (UserCode.directive_PROD_3_directive_SR1_PROD_3_ACT (env_RES, conf_RES, ASCII8_RES, KW_charset_RES),
                            strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.ASCII8, strm') => SR1_PROD_3(strm)
                      | (Tok.UTF8, strm') => SR1_PROD_1(strm)
                      | (Tok.ASCII7, strm') => SR1_PROD_2(strm)
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            in
              (wrap SR1_NT)(strm')
            end
            in
              (SR_RES, strm')
            end
      fun directive_PROD_4 (strm) = let
            val (KW_name_RES, strm') = matchKW_name(strm)
            val (ID_RES, strm') = matchID(strm')
            in
              (UserCode.directive_PROD_4_ACT (ID_RES, env_RES, conf_RES, KW_name_RES),
                strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_name, strm') => directive_PROD_4(strm)
          | (Tok.KW_states, strm') => directive_PROD_2(strm)
          | (Tok.KW_let, strm') => directive_PROD_1(strm)
          | (Tok.KW_charset, strm') => directive_PROD_3(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun decl_NT (spec_RES, env_RES) (strm) = let
      fun decl_PROD_1 (strm) = let
            val (directive_RES, strm') = (wrap (directive_NT (UserCode.ARGS_7 (env_RES, spec_RES))))(strm)
            in
              (UserCode.decl_PROD_1_ACT (env_RES, spec_RES, directive_RES),
                strm')
            end
      fun decl_PROD_2 (strm) = let
            val (KW_defs_RES, strm') = matchKW_defs(strm)
            val (CODE_RES, strm') = matchCODE(strm')
            in
              (UserCode.decl_PROD_2_ACT (env_RES, CODE_RES, spec_RES, KW_defs_RES),
                strm')
            end
      fun decl_PROD_3 (strm) = let
            val (rule_RES, strm') = (wrap (rule_NT (UserCode.ARGS_10 (env_RES, spec_RES))))(strm)
            in
              (UserCode.decl_PROD_3_ACT (env_RES, rule_RES, spec_RES), strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), strm') => decl_PROD_3(strm)
          | (Tok.UCHAR(_), strm') => decl_PROD_3(strm)
          | (Tok.CHAR(_), strm') => decl_PROD_3(strm)
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
fun decls_NT (spec_RES, env_RES) (strm) = let
      fun decls_PROD_1 (strm) = let
            val (decl_RES, strm') = (wrap (decl_NT (UserCode.ARGS_3 (env_RES, spec_RES))))(strm)
            val (SEMI_RES, strm') = matchSEMI(strm')
            val (decls_RES, strm') = (wrap (decls_NT (UserCode.ARGS_4 (env_RES, SEMI_RES, decl_RES, spec_RES))))(strm')
            in
              (UserCode.decls_PROD_1_ACT (env_RES, SEMI_RES, decl_RES, spec_RES, decls_RES),
                strm')
            end
      fun decls_PROD_2 (strm) = (UserCode.decls_PROD_2_ACT (env_RES, spec_RES),
              strm)
      in
        (case (lex(strm))
         of (Tok.EOF, strm') => decls_PROD_2(strm)
          | (Tok.ID(_), strm') => decls_PROD_1(strm)
          | (Tok.UCHAR(_), strm') => decls_PROD_1(strm)
          | (Tok.CHAR(_), strm') => decls_PROD_1(strm)
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
      val (decls_RES, strm') = (wrap (decls_NT (UserCode.ARGS_1 ())))(strm)
      val (EOF_RES, strm') = matchEOF(strm')
      in
        (decls_RES, strm')
      end
in
  (file_NT)
end
val file_NT =  fn s => unwrap (Err.launch eh (file_NT ) (WStream.wrap s))

in (file_NT) end

fun parse s = let val (file_NT) = mk() in file_NT s end


end (* structure Parser *)
