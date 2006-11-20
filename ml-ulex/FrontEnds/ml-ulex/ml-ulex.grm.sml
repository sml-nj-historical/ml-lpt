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

      type antlr_annotation = unit

 
  structure LS = LexSpec
  structure AMap = AtomMap

  structure S = LexSpec

  structure RE = RegExp
  structure SIS = RE.SymSet

  fun listToASet ls = AtomSet.addList (AtomSet.empty, ls)

  val wildcard = RE.mkSymSet (SIS.complement (SIS.singleton 0w10)) 
  fun charToSym c = Word32.fromInt (Char.ord c)

  fun flip (x, y) = (y, x)

  type antlr_annotation = Lex.span * string



fun decls_PROD_1_ACT (env, SEMI, decl, spec, decls, SEMI_SPAN : Lex.span, decl_SPAN : Lex.span, decls_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( decls)
fun decls_PROD_2_ACT (env, spec, addAnnotation, FULL_SPAN : Lex.span) = 
  ( spec)
fun decl_PROD_1_ACT (env, spec, directive, directive_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( let val (conf', env') = directive
	  in 
	    (LS.updConf (spec, conf'),
	     env')
	  end)
fun decl_PROD_2_ACT (env, CODE, spec, KW_defs, CODE_SPAN : Lex.span, KW_defs_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( LS.updDecls (spec, CODE), env)
fun decl_PROD_3_ACT (env, rule, spec, rule_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( LS.addRule (spec, rule), env)
fun directive_PROD_1_ACT (EQ, ID, re, env, conf, KW_let, EQ_SPAN : Lex.span, ID_SPAN : Lex.span, re_SPAN : Lex.span, KW_let_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( conf, AMap.insert (env, Atom.atom ID, re))
fun directive_PROD_2_directive_SR1_PROD_1_ACT (ID, env, conf, COMMA, KW_states, ID_SPAN : Lex.span, COMMA_SPAN : Lex.span, KW_states_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( ID)
fun directive_PROD_2_ACT (SR, env, conf, KW_states, SR_SPAN : Lex.span, KW_states_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( LS.updStartStates (conf, listToASet (map Atom.atom SR)), 
	  env)
fun directive_PROD_3_directive_SR1_PROD_1_ACT (env, UTF8, conf, KW_charset, UTF8_SPAN : Lex.span, KW_charset_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( LS.updClamp (conf, LS.NO_CLAMP), env)
fun directive_PROD_3_directive_SR1_PROD_2_ACT (env, conf, ASCII7, KW_charset, ASCII7_SPAN : Lex.span, KW_charset_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  LS.updClamp (conf, LS.CLAMP127), env)
fun directive_PROD_3_directive_SR1_PROD_3_ACT (env, conf, ASCII8, KW_charset, ASCII8_SPAN : Lex.span, KW_charset_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  LS.updClamp (conf, LS.CLAMP255), env)
fun directive_PROD_4_ACT (ID, env, conf, KW_name, ID_SPAN : Lex.span, KW_name_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( LS.updStructName (conf, ID), env)
fun rule_PROD_1_rule_SR1_PROD_1_SR1_SR1_PROD_1_ACT (ID, LT, env, COMMA, ID_SPAN : Lex.span, LT_SPAN : Lex.span, COMMA_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( ID)
fun rule_PROD_1_ACT (SR, re, env, CODE, DARROW, SR_SPAN : Lex.span, re_SPAN : Lex.span, CODE_SPAN : Lex.span, DARROW_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( (Option.map (listToASet o (map Atom.atom)) SR, re), CODE)
fun or_re_PROD_1_ACT (SR, env, and_re, SR_SPAN : Lex.span, and_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( foldl (RE.mkOr o flip) and_re SR)
fun and_re_PROD_1_ACT (SR, env, not_re, SR_SPAN : Lex.span, not_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( foldl (RE.mkAnd o flip) not_re SR)
fun not_re_PROD_1_ACT (env, cat_re, CARAT, cat_re_SPAN : Lex.span, CARAT_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( RE.mkNot cat_re)
fun cat_re_PROD_1_ACT (SR, env, post_re, SR_SPAN : Lex.span, post_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( foldl (RE.mkConcat o flip) post_re SR)
fun post_re_PROD_1_post_re_SR1_PROD_1_ACT (env, prim_re, QUERY, prim_re_SPAN : Lex.span, QUERY_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( RE.mkOpt)
fun post_re_PROD_1_post_re_SR1_PROD_2_ACT (env, STAR, prim_re, STAR_SPAN : Lex.span, prim_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( RE.mkClosure)
fun post_re_PROD_1_post_re_SR1_PROD_3_ACT (env, PLUS, prim_re, PLUS_SPAN : Lex.span, prim_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( fn re => RE.mkAtLeast (re, 1))
fun post_re_PROD_1_post_re_SR1_PROD_4_ACT (env, prim_re, REPEAT, prim_re_SPAN : Lex.span, REPEAT_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( fn re => RE.mkRep (re, REPEAT, REPEAT))
fun post_re_PROD_1_post_re_SR1_PROD_5_ACT (env, prim_re, prim_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( fn x => x)
fun post_re_PROD_1_ACT (SR, env, prim_re, SR_SPAN : Lex.span, prim_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( SR prim_re)
fun prim_re_PROD_1_ACT (ID, env, ID_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( case (AMap.find (env, Atom.atom ID))
	   of SOME re => re
	    | NONE => (addAnnotation (ID_SPAN, String.concat [
		"Error: {", ID, "} is undefined."]);
		RE.any))
fun prim_re_PROD_3_ACT (env, char, char_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( RE.mkSym char)
fun prim_re_PROD_4_ACT (DOT, env, DOT_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( wildcard)
fun prim_re_PROD_5_prim_re_SR1_PROD_1_ACT (LSB, env, CARAT, LSB_SPAN : Lex.span, CARAT_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( SIS.complement)
fun prim_re_PROD_5_prim_re_SR1_PROD_2_ACT (LSB, env, LSB_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( fn x => x)
fun prim_re_PROD_5_prim_re_SR2_PROD_1_ACT (LSB, SR1, env, DASH, char1, char2, LSB_SPAN : Lex.span, SR1_SPAN : Lex.span, DASH_SPAN : Lex.span, char1_SPAN : Lex.span, char2_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( 
	    if char1 <= char2 then
	       SIS.interval (char1, char2)
	     else (addAnnotation (FULL_SPAN, String.concat [
	       "Error: malformed character class: ",
	       Word32.toString char1, " - ",
	       Word32.toString char2, "."]);
	       SIS.universe))
fun prim_re_PROD_5_prim_re_SR2_PROD_2_ACT (LSB, SR1, env, char, LSB_SPAN : Lex.span, SR1_SPAN : Lex.span, char_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( SIS.singleton char)
fun prim_re_PROD_5_ACT (LSB, RSB, SR1, SR2, env, LSB_SPAN : Lex.span, RSB_SPAN : Lex.span, SR1_SPAN : Lex.span, SR2_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  ( RE.mkSymSet (SR1 (foldl SIS.union (hd SR2) (tl SR2))))
fun char_PROD_1_ACT (CHAR, CHAR_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
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

    val allRepairs = [Deletion, Insertion Tok.EOF, Insertion Tok.BOGUS, Insertion Tok.ASCII8, Insertion Tok.ASCII7, Insertion Tok.UTF8, Insertion Tok.KW_charset, Insertion Tok.KW_let, Insertion Tok.KW_states, Insertion Tok.KW_name, Insertion Tok.KW_defs, Insertion Tok.EQ, Insertion Tok.DARROW, Insertion Tok.DASH, Insertion Tok.CARAT, Insertion Tok.COMMA, Insertion Tok.SLASH, Insertion Tok.GT, Insertion Tok.LT, Insertion Tok.RCB, Insertion Tok.LCB, Insertion Tok.RSB, Insertion Tok.LSB, Insertion Tok.RP, Insertion Tok.LP, Insertion Tok.SEMI, Insertion Tok.QUERY, Insertion Tok.STAR, Insertion Tok.PLUS, Insertion Tok.DOLLAR, Insertion Tok.DOT, Insertion Tok.AMP, Insertion Tok.BAR, Substitution Tok.EOF, Substitution Tok.BOGUS, Substitution Tok.ASCII8, Substitution Tok.ASCII7, Substitution Tok.UTF8, Substitution Tok.KW_charset, Substitution Tok.KW_let, Substitution Tok.KW_states, Substitution Tok.KW_name, Substitution Tok.KW_defs, Substitution Tok.EQ, Substitution Tok.DARROW, Substitution Tok.DASH, Substitution Tok.CARAT, Substitution Tok.COMMA, Substitution Tok.SLASH, Substitution Tok.GT, Substitution Tok.LT, Substitution Tok.RCB, Substitution Tok.LCB, Substitution Tok.RSB, Substitution Tok.LSB, Substitution Tok.RP, Substitution Tok.LP, Substitution Tok.SEMI, Substitution Tok.QUERY, Substitution Tok.STAR, Substitution Tok.PLUS, Substitution Tok.DOLLAR, Substitution Tok.DOT, Substitution Tok.AMP, Substitution Tok.BAR]


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
val matchBOGUS = wrap (fn strm => (case (lex(strm))
 of (Tok.BOGUS, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCODE = wrap (fn strm => (case (lex(strm))
 of (Tok.CODE(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchID = wrap (fn strm => (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchUCHAR = wrap (fn strm => (case (lex(strm))
 of (Tok.UCHAR(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCHAR = wrap (fn strm => (case (lex(strm))
 of (Tok.CHAR(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchREPEAT = wrap (fn strm => (case (lex(strm))
 of (Tok.REPEAT(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchASCII8 = wrap (fn strm => (case (lex(strm))
 of (Tok.ASCII8, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchASCII7 = wrap (fn strm => (case (lex(strm))
 of (Tok.ASCII7, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchUTF8 = wrap (fn strm => (case (lex(strm))
 of (Tok.UTF8, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_charset = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_charset, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_let = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_let, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_states = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_states, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_name = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_name, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_defs = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_defs, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchEQ = wrap (fn strm => (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDARROW = wrap (fn strm => (case (lex(strm))
 of (Tok.DARROW, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDASH = wrap (fn strm => (case (lex(strm))
 of (Tok.DASH, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCARAT = wrap (fn strm => (case (lex(strm))
 of (Tok.CARAT, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCOMMA = wrap (fn strm => (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSLASH = wrap (fn strm => (case (lex(strm))
 of (Tok.SLASH, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchGT = wrap (fn strm => (case (lex(strm))
 of (Tok.GT, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchLT = wrap (fn strm => (case (lex(strm))
 of (Tok.LT, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchRCB = wrap (fn strm => (case (lex(strm))
 of (Tok.RCB, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchLCB = wrap (fn strm => (case (lex(strm))
 of (Tok.LCB, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchRSB = wrap (fn strm => (case (lex(strm))
 of (Tok.RSB, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchLSB = wrap (fn strm => (case (lex(strm))
 of (Tok.LSB, span, strm') => ((), span, strm')
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
val matchSEMI = wrap (fn strm => (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchQUERY = wrap (fn strm => (case (lex(strm))
 of (Tok.QUERY, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSTAR = wrap (fn strm => (case (lex(strm))
 of (Tok.STAR, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchPLUS = wrap (fn strm => (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDOLLAR = wrap (fn strm => (case (lex(strm))
 of (Tok.DOLLAR, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDOT = wrap (fn strm => (case (lex(strm))
 of (Tok.DOT, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchAMP = wrap (fn strm => (case (lex(strm))
 of (Tok.AMP, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchBAR = wrap (fn strm => (case (lex(strm))
 of (Tok.BAR, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))

val (file_NT) = 
let
fun char_NT (strm) = let
      fun char_PROD_1 (strm) = let
            val (CHAR_RES, CHAR_SPAN, strm') = matchCHAR(strm)
            val FULL_SPAN = (#1(CHAR_SPAN), #2(CHAR_SPAN))
            in
              (UserCode.char_PROD_1_ACT (CHAR_RES, CHAR_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun char_PROD_2 (strm) = let
            val (UCHAR_RES, UCHAR_SPAN, strm') = matchUCHAR(strm)
            val FULL_SPAN = (#1(UCHAR_SPAN), #2(UCHAR_SPAN))
            in
              (UCHAR_RES, FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.UCHAR(_), _, strm') => char_PROD_2(strm)
          | (Tok.CHAR(_), _, strm') => char_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun re_NT (env_RES) (strm) = let
      val (or_re_RES, or_re_SPAN, strm') = (wrap (or_re_NT (UserCode.ARGS_22 (env_RES))))(strm)
      val FULL_SPAN = (#1(or_re_SPAN), #2(or_re_SPAN))
      in
        (or_re_RES, FULL_SPAN, strm')
      end
and or_re_NT (env_RES) (strm) = let
      val (and_re_RES, and_re_SPAN, strm') = (wrap (and_re_NT (UserCode.ARGS_24 (env_RES))))(strm)
      fun SR1_NT (strm) = let
            val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
            val (and_re_RES, and_re_SPAN, strm') = (wrap (and_re_NT (UserCode.ARGS_25 (BAR_RES, env_RES, and_re_RES))))(strm')
            val FULL_SPAN = (#1(BAR_SPAN), #2(and_re_SPAN))
            in
              (and_re_RES, FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.BAR, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      val FULL_SPAN = (#1(and_re_SPAN), #2(SR_SPAN))
      in
        (UserCode.or_re_PROD_1_ACT (SR_RES, env_RES, and_re_RES, SR_SPAN : Lex.span, and_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
and and_re_NT (env_RES) (strm) = let
      val (not_re_RES, not_re_SPAN, strm') = (wrap (not_re_NT (UserCode.ARGS_27 (env_RES))))(strm)
      fun SR1_NT (strm) = let
            val (AMP_RES, AMP_SPAN, strm') = matchAMP(strm)
            val (not_re_RES, not_re_SPAN, strm') = (wrap (not_re_NT (UserCode.ARGS_28 (AMP_RES, env_RES, not_re_RES))))(strm')
            val FULL_SPAN = (#1(AMP_SPAN), #2(not_re_SPAN))
            in
              (not_re_RES, FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.AMP, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      val FULL_SPAN = (#1(not_re_SPAN), #2(SR_SPAN))
      in
        (UserCode.and_re_PROD_1_ACT (SR_RES, env_RES, not_re_RES, SR_SPAN : Lex.span, not_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
and not_re_NT (env_RES) (strm) = let
      fun not_re_PROD_1 (strm) = let
            val (CARAT_RES, CARAT_SPAN, strm') = matchCARAT(strm)
            val (cat_re_RES, cat_re_SPAN, strm') = (wrap (cat_re_NT (UserCode.ARGS_30 (env_RES, CARAT_RES))))(strm')
            val FULL_SPAN = (#1(CARAT_SPAN), #2(cat_re_SPAN))
            in
              (UserCode.not_re_PROD_1_ACT (env_RES, cat_re_RES, CARAT_RES, cat_re_SPAN : Lex.span, CARAT_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun not_re_PROD_2 (strm) = let
            val (cat_re_RES, cat_re_SPAN, strm') = (wrap (cat_re_NT (UserCode.ARGS_31 (env_RES))))(strm)
            val FULL_SPAN = (#1(cat_re_SPAN), #2(cat_re_SPAN))
            in
              (cat_re_RES, FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => not_re_PROD_2(strm)
          | (Tok.UCHAR(_), _, strm') => not_re_PROD_2(strm)
          | (Tok.CHAR(_), _, strm') => not_re_PROD_2(strm)
          | (Tok.LSB, _, strm') => not_re_PROD_2(strm)
          | (Tok.LP, _, strm') => not_re_PROD_2(strm)
          | (Tok.DOT, _, strm') => not_re_PROD_2(strm)
          | (Tok.CARAT, _, strm') => not_re_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
and cat_re_NT (env_RES) (strm) = let
      val (post_re_RES, post_re_SPAN, strm') = (wrap (post_re_NT (UserCode.ARGS_33 (env_RES))))(strm)
      fun SR1_NT (strm) = let
            val (post_re_RES, post_re_SPAN, strm') = (wrap (post_re_NT (UserCode.ARGS_34 (env_RES, post_re_RES))))(strm)
            val FULL_SPAN = (#1(post_re_SPAN), #2(post_re_SPAN))
            in
              (post_re_RES, FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | (Tok.UCHAR(_), _, strm') => true
              | (Tok.CHAR(_), _, strm') => true
              | (Tok.LSB, _, strm') => true
              | (Tok.LP, _, strm') => true
              | (Tok.DOT, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      val FULL_SPAN = (#1(post_re_SPAN), #2(SR_SPAN))
      in
        (UserCode.cat_re_PROD_1_ACT (SR_RES, env_RES, post_re_RES, SR_SPAN : Lex.span, post_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
and post_re_NT (env_RES) (strm) = let
      val (prim_re_RES, prim_re_SPAN, strm') = (wrap (prim_re_NT (UserCode.ARGS_36 (env_RES))))(strm)
      val (SR_RES, SR_SPAN, strm') = let
      fun SR1_NT (strm) = let
            fun SR1_PROD_1 (strm) = let
                  val (QUERY_RES, QUERY_SPAN, strm') = matchQUERY(strm)
                  val FULL_SPAN = (#1(QUERY_SPAN), #2(QUERY_SPAN))
                  in
                    (UserCode.post_re_PROD_1_post_re_SR1_PROD_1_ACT (env_RES, prim_re_RES, QUERY_RES, prim_re_SPAN : Lex.span, QUERY_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                      FULL_SPAN, strm')
                  end
            fun SR1_PROD_2 (strm) = let
                  val (STAR_RES, STAR_SPAN, strm') = matchSTAR(strm)
                  val FULL_SPAN = (#1(STAR_SPAN), #2(STAR_SPAN))
                  in
                    (UserCode.post_re_PROD_1_post_re_SR1_PROD_2_ACT (env_RES, STAR_RES, prim_re_RES, STAR_SPAN : Lex.span, prim_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                      FULL_SPAN, strm')
                  end
            fun SR1_PROD_3 (strm) = let
                  val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
                  val FULL_SPAN = (#1(PLUS_SPAN), #2(PLUS_SPAN))
                  in
                    (UserCode.post_re_PROD_1_post_re_SR1_PROD_3_ACT (env_RES, PLUS_RES, prim_re_RES, PLUS_SPAN : Lex.span, prim_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                      FULL_SPAN, strm')
                  end
            fun SR1_PROD_4 (strm) = let
                  val (REPEAT_RES, REPEAT_SPAN, strm') = matchREPEAT(strm)
                  val FULL_SPAN = (#1(REPEAT_SPAN), #2(REPEAT_SPAN))
                  in
                    (UserCode.post_re_PROD_1_post_re_SR1_PROD_4_ACT (env_RES, prim_re_RES, REPEAT_RES, prim_re_SPAN : Lex.span, REPEAT_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                      FULL_SPAN, strm')
                  end
            fun SR1_PROD_5 (strm) = let
                  val FULL_SPAN = (WStream.getPos(strm), WStream.getPos(strm))
                  in
                    (UserCode.post_re_PROD_1_post_re_SR1_PROD_5_ACT (env_RES, prim_re_RES, prim_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                      FULL_SPAN, strm)
                  end
            in
              (case (lex(strm))
               of (Tok.ID(_), _, strm') => SR1_PROD_5(strm)
                | (Tok.UCHAR(_), _, strm') => SR1_PROD_5(strm)
                | (Tok.CHAR(_), _, strm') => SR1_PROD_5(strm)
                | (Tok.DARROW, _, strm') => SR1_PROD_5(strm)
                | (Tok.LSB, _, strm') => SR1_PROD_5(strm)
                | (Tok.RP, _, strm') => SR1_PROD_5(strm)
                | (Tok.LP, _, strm') => SR1_PROD_5(strm)
                | (Tok.SEMI, _, strm') => SR1_PROD_5(strm)
                | (Tok.DOT, _, strm') => SR1_PROD_5(strm)
                | (Tok.AMP, _, strm') => SR1_PROD_5(strm)
                | (Tok.BAR, _, strm') => SR1_PROD_5(strm)
                | (Tok.PLUS, _, strm') => SR1_PROD_3(strm)
                | (Tok.QUERY, _, strm') => SR1_PROD_1(strm)
                | (Tok.STAR, _, strm') => SR1_PROD_2(strm)
                | (Tok.REPEAT(_), _, strm') => SR1_PROD_4(strm)
                | _ => raise(ParseError)
              (* end case *))
            end
      in
        (wrap SR1_NT)(strm')
      end
      val FULL_SPAN = (#1(prim_re_SPAN), #2(SR_SPAN))
      in
        (UserCode.post_re_PROD_1_ACT (SR_RES, env_RES, prim_re_RES, SR_SPAN : Lex.span, prim_re_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
and prim_re_NT (env_RES) (strm) = let
      fun prim_re_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.prim_re_PROD_1_ACT (ID_RES, env_RES, ID_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun prim_re_PROD_2 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (re_RES, re_SPAN, strm') = (wrap (re_NT (UserCode.ARGS_43 (LP_RES, env_RES))))(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (re_RES, FULL_SPAN, strm')
            end
      fun prim_re_PROD_3 (strm) = let
            val (char_RES, char_SPAN, strm') = (wrap char_NT)(strm)
            val FULL_SPAN = (#1(char_SPAN), #2(char_SPAN))
            in
              (UserCode.prim_re_PROD_3_ACT (env_RES, char_RES, char_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun prim_re_PROD_4 (strm) = let
            val (DOT_RES, DOT_SPAN, strm') = matchDOT(strm)
            val FULL_SPAN = (#1(DOT_SPAN), #2(DOT_SPAN))
            in
              (UserCode.prim_re_PROD_4_ACT (DOT_RES, env_RES, DOT_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun prim_re_PROD_5 (strm) = let
            val (LSB_RES, LSB_SPAN, strm') = matchLSB(strm)
            val (SR1_RES, SR1_SPAN, strm') = let
            fun SR1_NT (strm) = let
                  fun SR1_PROD_1 (strm) = let
                        val (CARAT_RES, CARAT_SPAN, strm') = matchCARAT(strm)
                        val FULL_SPAN = (#1(CARAT_SPAN), #2(CARAT_SPAN))
                        in
                          (UserCode.prim_re_PROD_5_prim_re_SR1_PROD_1_ACT (LSB_RES, env_RES, CARAT_RES, LSB_SPAN : Lex.span, CARAT_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                            FULL_SPAN, strm')
                        end
                  fun SR1_PROD_2 (strm) = let
                        val FULL_SPAN = (WStream.getPos(strm),
                          WStream.getPos(strm))
                        in
                          (UserCode.prim_re_PROD_5_prim_re_SR1_PROD_2_ACT (LSB_RES, env_RES, LSB_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                            FULL_SPAN, strm)
                        end
                  in
                    (case (lex(strm))
                     of (Tok.UCHAR(_), _, strm') => SR1_PROD_2(strm)
                      | (Tok.CHAR(_), _, strm') => SR1_PROD_2(strm)
                      | (Tok.CARAT, _, strm') => SR1_PROD_1(strm)
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            in
              (wrap SR1_NT)(strm')
            end
            fun SR2_NT (strm) = let
                  fun SR2_PROD_1 (strm) = let
                        val (char1_RES, char1_SPAN, strm') = (wrap char_NT)(strm)
                        val (DASH_RES, DASH_SPAN, strm') = matchDASH(strm')
                        val (char2_RES, char2_SPAN, strm') = (wrap char_NT)(strm')
                        val FULL_SPAN = (#1(char1_SPAN), #2(char2_SPAN))
                        in
                          (UserCode.prim_re_PROD_5_prim_re_SR2_PROD_1_ACT (LSB_RES, SR1_RES, env_RES, DASH_RES, char1_RES, char2_RES, LSB_SPAN : Lex.span, SR1_SPAN : Lex.span, DASH_SPAN : Lex.span, char1_SPAN : Lex.span, char2_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                            FULL_SPAN, strm')
                        end
                  fun SR2_PROD_2 (strm) = let
                        val (char_RES, char_SPAN, strm') = (wrap char_NT)(strm)
                        val FULL_SPAN = (#1(char_SPAN), #2(char_SPAN))
                        in
                          (UserCode.prim_re_PROD_5_prim_re_SR2_PROD_2_ACT (LSB_RES, SR1_RES, env_RES, char_RES, LSB_SPAN : Lex.span, SR1_SPAN : Lex.span, char_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.UCHAR(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.DASH, _, strm') => SR2_PROD_1(strm)
                            | (Tok.UCHAR(_), _, strm') => SR2_PROD_2(strm)
                            | (Tok.CHAR(_), _, strm') => SR2_PROD_2(strm)
                            | (Tok.RSB, _, strm') => SR2_PROD_2(strm)
                            | _ => raise(ParseError)
                          (* end case *))
                      | (Tok.CHAR(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.DASH, _, strm') => SR2_PROD_1(strm)
                            | (Tok.UCHAR(_), _, strm') => SR2_PROD_2(strm)
                            | (Tok.CHAR(_), _, strm') => SR2_PROD_2(strm)
                            | (Tok.RSB, _, strm') => SR2_PROD_2(strm)
                            | _ => raise(ParseError)
                          (* end case *))
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            fun SR2_PRED (strm) = (case (lex(strm))
                   of (Tok.UCHAR(_), _, strm') => true
                    | (Tok.CHAR(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR2_RES, SR2_SPAN, strm') = EBNF.posclos(SR2_PRED, (wrap SR2_NT), strm')
            val (RSB_RES, RSB_SPAN, strm') = matchRSB(strm')
            val FULL_SPAN = (#1(LSB_SPAN), #2(RSB_SPAN))
            in
              (UserCode.prim_re_PROD_5_ACT (LSB_RES, RSB_RES, SR1_RES, SR2_RES, env_RES, LSB_SPAN : Lex.span, RSB_SPAN : Lex.span, SR1_SPAN : Lex.span, SR2_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LSB, _, strm') => prim_re_PROD_5(strm)
          | (Tok.UCHAR(_), _, strm') => prim_re_PROD_3(strm)
          | (Tok.CHAR(_), _, strm') => prim_re_PROD_3(strm)
          | (Tok.ID(_), _, strm') => prim_re_PROD_1(strm)
          | (Tok.LP, _, strm') => prim_re_PROD_2(strm)
          | (Tok.DOT, _, strm') => prim_re_PROD_4(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun rule_NT (env_RES) (strm) = let
      fun SR1_NT (strm) = let
            val (LT_RES, LT_SPAN, strm') = matchLT(strm)
            fun SR1_NT (strm) = let
                  val (ID_RES, ID_SPAN, strm') = matchID(strm)
                  fun SR1_NT (strm) = let
                        val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                        val FULL_SPAN = (#1(COMMA_SPAN), #2(COMMA_SPAN))
                        in
                          ((), FULL_SPAN, strm')
                        end
                  fun SR1_PRED (strm) = (case (lex(strm))
                         of (Tok.COMMA, _, strm') => true
                          | _ => false
                        (* end case *))
                  val (COMMA_RES, COMMA_SPAN, strm') = EBNF.optional(SR1_PRED, (wrap SR1_NT), strm')
                  val FULL_SPAN = (#1(ID_SPAN), #2(COMMA_SPAN))
                  in
                    (UserCode.rule_PROD_1_rule_SR1_PROD_1_SR1_SR1_PROD_1_ACT (ID_RES, LT_RES, env_RES, COMMA_RES, ID_SPAN : Lex.span, LT_SPAN : Lex.span, COMMA_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                      FULL_SPAN, strm')
                  end
            fun SR1_PRED (strm) = (case (lex(strm))
                   of (Tok.ID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.posclos(SR1_PRED, (wrap SR1_NT), strm')
            val (GT_RES, GT_SPAN, strm') = matchGT(strm')
            val FULL_SPAN = (#1(LT_SPAN), #2(GT_SPAN))
            in
              (SR_RES, FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.LT, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(SR1_PRED, (wrap SR1_NT), strm)
      val (re_RES, re_SPAN, strm') = (wrap (re_NT (UserCode.ARGS_21 (SR_RES, env_RES))))(strm')
      val (DARROW_RES, DARROW_SPAN, strm') = matchDARROW(strm')
      val (CODE_RES, CODE_SPAN, strm') = matchCODE(strm')
      val FULL_SPAN = (#1(SR_SPAN), #2(CODE_SPAN))
      in
        (UserCode.rule_PROD_1_ACT (SR_RES, re_RES, env_RES, CODE_RES, DARROW_RES, SR_SPAN : Lex.span, re_SPAN : Lex.span, CODE_SPAN : Lex.span, DARROW_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
fun directive_NT (conf_RES, env_RES) (strm) = let
      fun directive_PROD_1 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (re_RES, re_SPAN, strm') = (wrap (re_NT (UserCode.ARGS_12 (EQ_RES, ID_RES, env_RES, conf_RES, KW_let_RES))))(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(re_SPAN))
            in
              (UserCode.directive_PROD_1_ACT (EQ_RES, ID_RES, re_RES, env_RES, conf_RES, KW_let_RES, EQ_SPAN : Lex.span, ID_SPAN : Lex.span, re_SPAN : Lex.span, KW_let_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun directive_PROD_2 (strm) = let
            val (KW_states_RES, KW_states_SPAN, strm') = matchKW_states(strm)
            fun SR1_NT (strm) = let
                  val (ID_RES, ID_SPAN, strm') = matchID(strm)
                  fun SR1_NT (strm) = let
                        val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                        val FULL_SPAN = (#1(COMMA_SPAN), #2(COMMA_SPAN))
                        in
                          ((), FULL_SPAN, strm')
                        end
                  fun SR1_PRED (strm) = (case (lex(strm))
                         of (Tok.COMMA, _, strm') => true
                          | _ => false
                        (* end case *))
                  val (COMMA_RES, COMMA_SPAN, strm') = EBNF.optional(SR1_PRED, (wrap SR1_NT), strm')
                  val FULL_SPAN = (#1(ID_SPAN), #2(COMMA_SPAN))
                  in
                    (UserCode.directive_PROD_2_directive_SR1_PROD_1_ACT (ID_RES, env_RES, conf_RES, COMMA_RES, KW_states_RES, ID_SPAN : Lex.span, COMMA_SPAN : Lex.span, KW_states_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                      FULL_SPAN, strm')
                  end
            fun SR1_PRED (strm) = (case (lex(strm))
                   of (Tok.ID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.posclos(SR1_PRED, (wrap SR1_NT), strm')
            val FULL_SPAN = (#1(KW_states_SPAN), #2(SR_SPAN))
            in
              (UserCode.directive_PROD_2_ACT (SR_RES, env_RES, conf_RES, KW_states_RES, SR_SPAN : Lex.span, KW_states_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun directive_PROD_3 (strm) = let
            val (KW_charset_RES, KW_charset_SPAN, strm') = matchKW_charset(strm)
            val (SR_RES, SR_SPAN, strm') = let
            fun SR1_NT (strm) = let
                  fun SR1_PROD_1 (strm) = let
                        val (UTF8_RES, UTF8_SPAN, strm') = matchUTF8(strm)
                        val FULL_SPAN = (#1(UTF8_SPAN), #2(UTF8_SPAN))
                        in
                          (UserCode.directive_PROD_3_directive_SR1_PROD_1_ACT (env_RES, UTF8_RES, conf_RES, KW_charset_RES, UTF8_SPAN : Lex.span, KW_charset_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                            FULL_SPAN, strm')
                        end
                  fun SR1_PROD_2 (strm) = let
                        val (ASCII7_RES, ASCII7_SPAN, strm') = matchASCII7(strm)
                        val FULL_SPAN = (#1(ASCII7_SPAN), #2(ASCII7_SPAN))
                        in
                          (UserCode.directive_PROD_3_directive_SR1_PROD_2_ACT (env_RES, conf_RES, ASCII7_RES, KW_charset_RES, ASCII7_SPAN : Lex.span, KW_charset_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                            FULL_SPAN, strm')
                        end
                  fun SR1_PROD_3 (strm) = let
                        val (ASCII8_RES, ASCII8_SPAN, strm') = matchASCII8(strm)
                        val FULL_SPAN = (#1(ASCII8_SPAN), #2(ASCII8_SPAN))
                        in
                          (UserCode.directive_PROD_3_directive_SR1_PROD_3_ACT (env_RES, conf_RES, ASCII8_RES, KW_charset_RES, ASCII8_SPAN : Lex.span, KW_charset_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.ASCII8, _, strm') => SR1_PROD_3(strm)
                      | (Tok.UTF8, _, strm') => SR1_PROD_1(strm)
                      | (Tok.ASCII7, _, strm') => SR1_PROD_2(strm)
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            in
              (wrap SR1_NT)(strm')
            end
            val FULL_SPAN = (#1(KW_charset_SPAN), #2(SR_SPAN))
            in
              (SR_RES, FULL_SPAN, strm')
            end
      fun directive_PROD_4 (strm) = let
            val (KW_name_RES, KW_name_SPAN, strm') = matchKW_name(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val FULL_SPAN = (#1(KW_name_SPAN), #2(ID_SPAN))
            in
              (UserCode.directive_PROD_4_ACT (ID_RES, env_RES, conf_RES, KW_name_RES, ID_SPAN : Lex.span, KW_name_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_name, _, strm') => directive_PROD_4(strm)
          | (Tok.KW_states, _, strm') => directive_PROD_2(strm)
          | (Tok.KW_let, _, strm') => directive_PROD_1(strm)
          | (Tok.KW_charset, _, strm') => directive_PROD_3(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun decl_NT (spec_RES, env_RES) (strm) = let
      fun decl_PROD_1 (strm) = let
            val (directive_RES, directive_SPAN, strm') = (wrap (directive_NT (UserCode.ARGS_7 (env_RES, spec_RES))))(strm)
            val FULL_SPAN = (#1(directive_SPAN), #2(directive_SPAN))
            in
              (UserCode.decl_PROD_1_ACT (env_RES, spec_RES, directive_RES, directive_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun decl_PROD_2 (strm) = let
            val (KW_defs_RES, KW_defs_SPAN, strm') = matchKW_defs(strm)
            val (CODE_RES, CODE_SPAN, strm') = matchCODE(strm')
            val FULL_SPAN = (#1(KW_defs_SPAN), #2(CODE_SPAN))
            in
              (UserCode.decl_PROD_2_ACT (env_RES, CODE_RES, spec_RES, KW_defs_RES, CODE_SPAN : Lex.span, KW_defs_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun decl_PROD_3 (strm) = let
            val (rule_RES, rule_SPAN, strm') = (wrap (rule_NT (UserCode.ARGS_10 (env_RES, spec_RES))))(strm)
            val FULL_SPAN = (#1(rule_SPAN), #2(rule_SPAN))
            in
              (UserCode.decl_PROD_3_ACT (env_RES, rule_RES, spec_RES, rule_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => decl_PROD_3(strm)
          | (Tok.UCHAR(_), _, strm') => decl_PROD_3(strm)
          | (Tok.CHAR(_), _, strm') => decl_PROD_3(strm)
          | (Tok.CARAT, _, strm') => decl_PROD_3(strm)
          | (Tok.LT, _, strm') => decl_PROD_3(strm)
          | (Tok.LSB, _, strm') => decl_PROD_3(strm)
          | (Tok.LP, _, strm') => decl_PROD_3(strm)
          | (Tok.DOT, _, strm') => decl_PROD_3(strm)
          | (Tok.KW_charset, _, strm') => decl_PROD_1(strm)
          | (Tok.KW_let, _, strm') => decl_PROD_1(strm)
          | (Tok.KW_states, _, strm') => decl_PROD_1(strm)
          | (Tok.KW_name, _, strm') => decl_PROD_1(strm)
          | (Tok.KW_defs, _, strm') => decl_PROD_2(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun decls_NT (spec_RES, env_RES) (strm) = let
      fun decls_PROD_1 (strm) = let
            val (decl_RES, decl_SPAN, strm') = (wrap (decl_NT (UserCode.ARGS_3 (env_RES, spec_RES))))(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (decls_RES, decls_SPAN, strm') = (wrap (decls_NT (UserCode.ARGS_4 (env_RES, SEMI_RES, decl_RES, spec_RES))))(strm')
            val FULL_SPAN = (#1(decl_SPAN), #2(decls_SPAN))
            in
              (UserCode.decls_PROD_1_ACT (env_RES, SEMI_RES, decl_RES, spec_RES, decls_RES, SEMI_SPAN : Lex.span, decl_SPAN : Lex.span, decls_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun decls_PROD_2 (strm) = let
            val FULL_SPAN = (WStream.getPos(strm), WStream.getPos(strm))
            in
              (UserCode.decls_PROD_2_ACT (env_RES, spec_RES, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm)
            end
      in
        (case (lex(strm))
         of (Tok.EOF, _, strm') => decls_PROD_2(strm)
          | (Tok.ID(_), _, strm') => decls_PROD_1(strm)
          | (Tok.UCHAR(_), _, strm') => decls_PROD_1(strm)
          | (Tok.CHAR(_), _, strm') => decls_PROD_1(strm)
          | (Tok.KW_charset, _, strm') => decls_PROD_1(strm)
          | (Tok.KW_let, _, strm') => decls_PROD_1(strm)
          | (Tok.KW_states, _, strm') => decls_PROD_1(strm)
          | (Tok.KW_name, _, strm') => decls_PROD_1(strm)
          | (Tok.KW_defs, _, strm') => decls_PROD_1(strm)
          | (Tok.CARAT, _, strm') => decls_PROD_1(strm)
          | (Tok.LT, _, strm') => decls_PROD_1(strm)
          | (Tok.LSB, _, strm') => decls_PROD_1(strm)
          | (Tok.LP, _, strm') => decls_PROD_1(strm)
          | (Tok.DOT, _, strm') => decls_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun file_NT (strm) = let
      val (decls_RES, decls_SPAN, strm') = (wrap (decls_NT (UserCode.ARGS_1 ())))(strm)
      val (EOF_RES, EOF_SPAN, strm') = matchEOF(strm')
      val FULL_SPAN = (#1(decls_SPAN), #2(EOF_SPAN))
      in
        (decls_RES, FULL_SPAN, strm')
      end
in
  (file_NT)
end
val file_NT =  fn s => unwrap (Err.launch eh (file_NT ) (WStream.wrap s))

in (file_NT) end

fun parse s = let val (file_NT) = mk() in file_NT s end


end (* structure Parser *)
