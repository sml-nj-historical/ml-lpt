structure 
MLULexTokens = struct

    datatype token = EOF
      | BOGUS
      | CODE of string
      | ID of string
      | UCHAR of UTF8.wchar
      | CHAR of char
      | INT of int
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

    val allToks = [EOF, BOGUS, ASCII8, ASCII7, UTF8, KW_charset, KW_let, KW_states, KW_name, KW_defs, EQ, DARROW, DASH, CARAT, COMMA, SLASH, GT, LT, RCB, LCB, RSB, LSB, RP, LP, SEMI, QUERY, STAR, PLUS, DOLLAR, DOT, AMP, BAR]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (BOGUS) => "BOGUS"
  | (CODE(_)) => "CODE"
  | (ID(_)) => "ID"
  | (UCHAR(_)) => "UCHAR"
  | (CHAR(_)) => "CHAR"
  | (INT(_)) => "INT"
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
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (BOGUS) => false
  | (CODE(_)) => false
  | (ID(_)) => false
  | (UCHAR(_)) => false
  | (CHAR(_)) => false
  | (INT(_)) => false
  | (ASCII8) => false
  | (ASCII7) => false
  | (UTF8) => false
  | (KW_charset) => true
  | (KW_let) => true
  | (KW_states) => true
  | (KW_name) => true
  | (KW_defs) => true
  | (EQ) => false
  | (DARROW) => false
  | (DASH) => false
  | (CARAT) => false
  | (COMMA) => false
  | (SLASH) => false
  | (GT) => false
  | (LT) => false
  | (RCB) => false
  | (LCB) => false
  | (RSB) => false
  | (LSB) => false
  | (RP) => false
  | (LP) => false
  | (SEMI) => false
  | (QUERY) => false
  | (STAR) => false
  | (PLUS) => false
  | (DOLLAR) => false
  | (DOT) => false
  | (AMP) => false
  | (BAR) => false
(* end case *))


  fun toksToString toks = String.concatWith " " (map toString toks)

  fun isEOF EOF = true
    | isEOF _ = false

end

functor MLULexParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
MLULexTokens
    structure UserCode = struct

 
  structure LS = LexSpec
  structure AMap = AtomMap

  structure S = LexSpec

  structure RE = RegExp
  structure SIS = RE.SymSet

  fun listToASet ls = AtomSet.addList (AtomSet.empty, ls)
  fun charToSym c = Word.fromInt (Char.ord c)
  val dashSet = SIS.singleton (charToSym #"-")

  fun flip (x, y) = (y, x)




fun decls_PROD_1_ACT (env, SEMI, decl, spec, decls, SEMI_SPAN : (Lex.pos * Lex.pos), decl_SPAN : (Lex.pos * Lex.pos), decls_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( decls)
fun decls_PROD_2_ACT (env, spec, FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( spec)
fun decl_PROD_1_ACT (env, spec, directive, directive_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( let val (conf', env') = directive
	  in 
	    (LS.updConf (spec, conf'),
	     env')
	  end)
fun decl_PROD_2_ACT (env, CODE, spec, KW_defs, CODE_SPAN : (Lex.pos * Lex.pos), KW_defs_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( LS.updDecls (spec, CODE), env)
fun decl_PROD_3_ACT (env, rule, spec, rule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( LS.addRule (spec, rule), env)
fun directive_PROD_1_ACT (EQ, ID, re, env, conf, KW_let, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), re_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( conf, AMap.insert (env, Atom.atom ID, re))
fun directive_PROD_2_directive_directive_PROD_2_SUBRULE_1_PROD_1_ACT (ID, env, conf, COMMA, KW_states, ID_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), KW_states_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( ID)
fun directive_PROD_2_ACT (SR, env, conf, KW_states, SR_SPAN : (Lex.pos * Lex.pos), KW_states_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( LS.updStartStates (conf, listToASet (map Atom.atom SR)), 
	  env)
fun directive_PROD_3_directive_directive_PROD_3_SUBRULE_1_PROD_1_ACT (env, UTF8, conf, KW_charset, UTF8_SPAN : (Lex.pos * Lex.pos), KW_charset_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( LS.updClamp (conf, LS.NO_CLAMP), env)
fun directive_PROD_3_directive_directive_PROD_3_SUBRULE_1_PROD_2_ACT (env, conf, ASCII7, KW_charset, ASCII7_SPAN : (Lex.pos * Lex.pos), KW_charset_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  (  LS.updClamp (conf, LS.CLAMP127), env)
fun directive_PROD_3_directive_directive_PROD_3_SUBRULE_1_PROD_3_ACT (env, conf, ASCII8, KW_charset, ASCII8_SPAN : (Lex.pos * Lex.pos), KW_charset_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  (  LS.updClamp (conf, LS.CLAMP255), env)
fun directive_PROD_4_ACT (ID, env, conf, KW_name, ID_SPAN : (Lex.pos * Lex.pos), KW_name_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( LS.updStructName (conf, ID), env)
fun rule_PROD_1_rule_rule_PROD_1_SUBRULE_1_PROD_1_rule_PROD_1_SUBRULE_1_rule_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (ID, LT, env, COMMA, ID_SPAN : (Lex.pos * Lex.pos), LT_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( ID)
fun rule_PROD_1_ACT (SR, re, env, CODE, DARROW, SR_SPAN : (Lex.pos * Lex.pos), re_SPAN : (Lex.pos * Lex.pos), CODE_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( (Option.map (listToASet o (map Atom.atom)) SR, re), CODE)
fun or_re_PROD_1_ACT (SR, env, and_re, SR_SPAN : (Lex.pos * Lex.pos), and_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( foldl (RE.mkOr o flip) and_re SR)
fun and_re_PROD_1_ACT (SR, env, not_re, SR_SPAN : (Lex.pos * Lex.pos), not_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( foldl (RE.mkAnd o flip) not_re SR)
fun not_re_PROD_1_ACT (env, cat_re, CARAT, cat_re_SPAN : (Lex.pos * Lex.pos), CARAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( RE.mkNot cat_re)
fun cat_re_PROD_1_ACT (SR, env, post_re, SR_SPAN : (Lex.pos * Lex.pos), post_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( foldl (RE.mkConcat o flip) post_re SR)
fun post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_1_ACT (env, prim_re, QUERY, prim_re_SPAN : (Lex.pos * Lex.pos), QUERY_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( RE.mkOpt)
fun post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_2_ACT (env, STAR, prim_re, STAR_SPAN : (Lex.pos * Lex.pos), prim_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( RE.mkClosure)
fun post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_3_ACT (env, PLUS, prim_re, PLUS_SPAN : (Lex.pos * Lex.pos), prim_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( fn re => RE.mkAtLeast (re, 1))
fun post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_4_ACT (INT, LCB, RCB, env, prim_re, INT_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), prim_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( fn re => RE.mkRep (re, INT, INT))
fun post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_5_ACT (LCB, RCB, env, INT1, INT2, prim_re, COMMA, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), INT1_SPAN : (Lex.pos * Lex.pos), INT2_SPAN : (Lex.pos * Lex.pos), prim_re_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( fn re => RE.mkRep (re, INT1, INT2))
fun post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_6_ACT (env, prim_re, prim_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( fn x => x)
fun post_re_PROD_1_ACT (SR, env, prim_re, SR_SPAN : (Lex.pos * Lex.pos), prim_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( SR prim_re)
fun prim_re_PROD_1_ACT (ID, LCB, RCB, env, ID_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( case (AMap.find (env, Atom.atom ID))
	   of SOME re => re
	    | NONE => (errs := (ID_SPAN, String.concat [
		"Error: {", ID, "} is undefined."])::(!errs);
		RE.any))
fun prim_re_PROD_3_ACT (env, char, char_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( RE.mkSym char)
fun prim_re_PROD_4_ACT (DOT, env, DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( RE.any)
fun prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_1_PROD_1_ACT (LSB, env, CARAT, LSB_SPAN : (Lex.pos * Lex.pos), CARAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( SIS.complement)
fun prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_1_PROD_2_ACT (LSB, env, DASH, LSB_SPAN : (Lex.pos * Lex.pos), DASH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( fn x => SIS.union (x, dashSet))
fun prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_1_PROD_3_ACT (LSB, env, LSB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( fn x => x)
fun prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_2_PROD_1_ACT (LSB, SR1, env, DASH, char1, char2, LSB_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), DASH_SPAN : (Lex.pos * Lex.pos), char1_SPAN : (Lex.pos * Lex.pos), char2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( 
	    if char1 <= char2 then
	       SIS.interval (char1, char2)
	     else (errs := (FULL_SPAN, String.concat [
	       "Error: malformed character class: ",
	       Word.toString char1, " - ",
	       Word.toString char2, "."])::(!errs);
	       SIS.universe))
fun prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_2_PROD_2_ACT (LSB, SR1, env, char, LSB_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), char_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( SIS.singleton char)
fun prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_3_PROD_1_ACT (LSB, SR1, SR2, env, DASH, LSB_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), DASH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( dashSet)
fun prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_3_PROD_2_ACT (LSB, SR1, SR2, env, LSB_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( SIS.empty)
fun prim_re_PROD_5_ACT (LSB, RSB, SR1, SR2, SR3, env, LSB_SPAN : (Lex.pos * Lex.pos), RSB_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), SR3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( RE.mkSymSet 
	    (SR1 (foldl SIS.union SR3 SR2)))
fun char_PROD_1_ACT (CHAR, CHAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs) = 
  ( charToSym CHAR)
fun ARGS_4 (errs) = 
  (LS.mkSpec(), AMap.empty)
fun ARGS_6 (env, spec, errs) = 
  (spec, env)
fun ARGS_7 (env, SEMI, decl, spec, errs) = 
  (decl)
fun ARGS_10 (env, spec, errs) = 
  (LS.getConf spec, env)
fun ARGS_13 (env, spec, errs) = 
  (env)
fun ARGS_15 (EQ, ID, env, conf, KW_let, errs) = 
  (env)
fun ARGS_24 (SR, env, errs) = 
  (env)
fun ARGS_25 (env, errs) = 
  (env)
fun ARGS_28 (BAR, env, and_re, errs) = 
  (env)
fun ARGS_27 (env, errs) = 
  (env)
fun ARGS_31 (AMP, env, not_re, errs) = 
  (env)
fun ARGS_30 (env, errs) = 
  (env)
fun ARGS_33 (env, CARAT, errs) = 
  (env)
fun ARGS_34 (env, errs) = 
  (env)
fun ARGS_37 (env, post_re, errs) = 
  (env)
fun ARGS_36 (env, errs) = 
  (env)
fun ARGS_39 (env, errs) = 
  (env)
fun ARGS_47 (LP, env, errs) = 
  (env)

    end

    structure R = RepairableStrm(Tok)(Lex)
    structure Err = ErrHandler(R)
    structure EBNF = EBNF(R)

    exception ParseError = Err.RepairableError

    fun mk lexFn = let
val errs_REFC : ((StreamPos.span * string) list) ref = ref ( [])
fun getS() = {errs = !errs_REFC}
fun putS{errs} = (errs_REFC := errs)
fun unwrap (ret, strm, repairs) = (ret, R.unwrap strm, repairs, getS())
        val eh = Err.mkErrHandler {get = getS, put = putS}
	fun wrap f = Err.wrap eh f
	fun reqEOF' f s = let
	      val (act, span, s') = f s
	      in (wrap (fn s' => (case R.get1 s'
		  of (Tok.EOF, _, _) => (act, span, s')
		   | _ => raise ParseError))) s'
	      end
	fun reqEOF f = wrap (reqEOF' f)
	val whileDisabled = Err.whileDisabled eh
	fun tryProds (strm, prods) = (wrap (Err.tryProds eh prods)) strm
	val lex = R.get1
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
val matchINT = wrap (fn strm => (case (lex(strm))
 of (Tok.INT(x), span, strm') => (x, span, strm')
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
              (UserCode.char_PROD_1_ACT (CHAR_RES, CHAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      fun char_PROD_2 (strm) = let
            val (UCHAR_RES, UCHAR_SPAN, strm') = matchUCHAR(strm)
            val FULL_SPAN = (#1(UCHAR_SPAN), #2(UCHAR_SPAN))
            in
              ((UCHAR_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.UCHAR(_), _, strm') => char_PROD_2(strm)
          | (Tok.CHAR(_), _, strm') => char_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun re_NT (env_RES) (strm) = let
      val (or_re_RES, or_re_SPAN, strm') = (wrap (or_re_NT (UserCode.ARGS_25 (env_RES, errs_REFC))))(strm)
      val FULL_SPAN = (#1(or_re_SPAN), #2(or_re_SPAN))
      in
        ((or_re_RES), FULL_SPAN, strm')
      end
and or_re_NT (env_RES) (strm) = let
      val (and_re_RES, and_re_SPAN, strm') = (wrap (and_re_NT (UserCode.ARGS_27 (env_RES, errs_REFC))))(strm)
      fun or_re_PROD_1_SUBRULE_1_NT (strm) = let
            val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
            val (and_re_RES, and_re_SPAN, strm') = (wrap (and_re_NT (UserCode.ARGS_28 (BAR_RES, env_RES, and_re_RES, errs_REFC))))(strm')
            val FULL_SPAN = (#1(BAR_SPAN), #2(and_re_SPAN))
            in
              ((and_re_RES), FULL_SPAN, strm')
            end
      fun or_re_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.BAR, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(or_re_PROD_1_SUBRULE_1_PRED, (wrap or_re_PROD_1_SUBRULE_1_NT), strm')
      val FULL_SPAN = (#1(and_re_SPAN), #2(SR_SPAN))
      in
        (UserCode.or_re_PROD_1_ACT (SR_RES, env_RES, and_re_RES, SR_SPAN : (Lex.pos * Lex.pos), and_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
          FULL_SPAN, strm')
      end
and and_re_NT (env_RES) (strm) = let
      val (not_re_RES, not_re_SPAN, strm') = (wrap (not_re_NT (UserCode.ARGS_30 (env_RES, errs_REFC))))(strm)
      fun and_re_PROD_1_SUBRULE_1_NT (strm) = let
            val (AMP_RES, AMP_SPAN, strm') = matchAMP(strm)
            val (not_re_RES, not_re_SPAN, strm') = (wrap (not_re_NT (UserCode.ARGS_31 (AMP_RES, env_RES, not_re_RES, errs_REFC))))(strm')
            val FULL_SPAN = (#1(AMP_SPAN), #2(not_re_SPAN))
            in
              ((not_re_RES), FULL_SPAN, strm')
            end
      fun and_re_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.AMP, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(and_re_PROD_1_SUBRULE_1_PRED, (wrap and_re_PROD_1_SUBRULE_1_NT), strm')
      val FULL_SPAN = (#1(not_re_SPAN), #2(SR_SPAN))
      in
        (UserCode.and_re_PROD_1_ACT (SR_RES, env_RES, not_re_RES, SR_SPAN : (Lex.pos * Lex.pos), not_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
          FULL_SPAN, strm')
      end
and not_re_NT (env_RES) (strm) = let
      fun not_re_PROD_1 (strm) = let
            val (CARAT_RES, CARAT_SPAN, strm') = matchCARAT(strm)
            val (cat_re_RES, cat_re_SPAN, strm') = (wrap (cat_re_NT (UserCode.ARGS_33 (env_RES, CARAT_RES, errs_REFC))))(strm')
            val FULL_SPAN = (#1(CARAT_SPAN), #2(cat_re_SPAN))
            in
              (UserCode.not_re_PROD_1_ACT (env_RES, cat_re_RES, CARAT_RES, cat_re_SPAN : (Lex.pos * Lex.pos), CARAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      fun not_re_PROD_2 (strm) = let
            val (cat_re_RES, cat_re_SPAN, strm') = (wrap (cat_re_NT (UserCode.ARGS_34 (env_RES, errs_REFC))))(strm)
            val FULL_SPAN = (#1(cat_re_SPAN), #2(cat_re_SPAN))
            in
              ((cat_re_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.DOT, _, strm') => not_re_PROD_2(strm)
          | (Tok.LP, _, strm') => not_re_PROD_2(strm)
          | (Tok.LSB, _, strm') => not_re_PROD_2(strm)
          | (Tok.LCB, _, strm') => not_re_PROD_2(strm)
          | (Tok.CHAR(_), _, strm') => not_re_PROD_2(strm)
          | (Tok.UCHAR(_), _, strm') => not_re_PROD_2(strm)
          | (Tok.CARAT, _, strm') => not_re_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
and cat_re_NT (env_RES) (strm) = let
      val (post_re_RES, post_re_SPAN, strm') = (wrap (post_re_NT (UserCode.ARGS_36 (env_RES, errs_REFC))))(strm)
      fun cat_re_PROD_1_SUBRULE_1_NT (strm) = let
            val (post_re_RES, post_re_SPAN, strm') = (wrap (post_re_NT (UserCode.ARGS_37 (env_RES, post_re_RES, errs_REFC))))(strm)
            val FULL_SPAN = (#1(post_re_SPAN), #2(post_re_SPAN))
            in
              ((post_re_RES), FULL_SPAN, strm')
            end
      fun cat_re_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.DOT, _, strm') => true
              | (Tok.LP, _, strm') => true
              | (Tok.LSB, _, strm') => true
              | (Tok.LCB, _, strm') => true
              | (Tok.CHAR(_), _, strm') => true
              | (Tok.UCHAR(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(cat_re_PROD_1_SUBRULE_1_PRED, (wrap cat_re_PROD_1_SUBRULE_1_NT), strm')
      val FULL_SPAN = (#1(post_re_SPAN), #2(SR_SPAN))
      in
        (UserCode.cat_re_PROD_1_ACT (SR_RES, env_RES, post_re_RES, SR_SPAN : (Lex.pos * Lex.pos), post_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
          FULL_SPAN, strm')
      end
and post_re_NT (env_RES) (strm) = let
      val (prim_re_RES, prim_re_SPAN, strm') = (wrap (prim_re_NT (UserCode.ARGS_39 (env_RES, errs_REFC))))(strm)
      val (SR_RES, SR_SPAN, strm') = let
      fun post_re_PROD_1_SUBRULE_1_NT (strm) = let
            fun post_re_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (QUERY_RES, QUERY_SPAN, strm') = matchQUERY(strm)
                  val FULL_SPAN = (#1(QUERY_SPAN), #2(QUERY_SPAN))
                  in
                    (UserCode.post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_1_ACT (env_RES, prim_re_RES, QUERY_RES, prim_re_SPAN : (Lex.pos * Lex.pos), QUERY_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                      FULL_SPAN, strm')
                  end
            fun post_re_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (STAR_RES, STAR_SPAN, strm') = matchSTAR(strm)
                  val FULL_SPAN = (#1(STAR_SPAN), #2(STAR_SPAN))
                  in
                    (UserCode.post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_2_ACT (env_RES, STAR_RES, prim_re_RES, STAR_SPAN : (Lex.pos * Lex.pos), prim_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                      FULL_SPAN, strm')
                  end
            fun post_re_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                  val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
                  val FULL_SPAN = (#1(PLUS_SPAN), #2(PLUS_SPAN))
                  in
                    (UserCode.post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_3_ACT (env_RES, PLUS_RES, prim_re_RES, PLUS_SPAN : (Lex.pos * Lex.pos), prim_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                      FULL_SPAN, strm')
                  end
            fun post_re_PROD_1_SUBRULE_1_PROD_4 (strm) = let
                  val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
                  val (INT_RES, INT_SPAN, strm') = matchINT(strm')
                  val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
                  val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
                  in
                    (UserCode.post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_4_ACT (INT_RES, LCB_RES, RCB_RES, env_RES, prim_re_RES, INT_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), prim_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                      FULL_SPAN, strm')
                  end
            fun post_re_PROD_1_SUBRULE_1_PROD_5 (strm) = let
                  val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
                  val (INT1_RES, INT1_SPAN, strm') = matchINT(strm')
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
                  val (INT2_RES, INT2_SPAN, strm') = matchINT(strm')
                  val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
                  val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
                  in
                    (UserCode.post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_5_ACT (LCB_RES, RCB_RES, env_RES, INT1_RES, INT2_RES, prim_re_RES, COMMA_RES, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), INT1_SPAN : (Lex.pos * Lex.pos), INT2_SPAN : (Lex.pos * Lex.pos), prim_re_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                      FULL_SPAN, strm')
                  end
            fun post_re_PROD_1_SUBRULE_1_PROD_6 (strm) = let
                  val FULL_SPAN = (R.getPos(strm), R.getPos(strm))
                  in
                    (UserCode.post_re_PROD_1_post_re_post_re_PROD_1_SUBRULE_1_PROD_6_ACT (env_RES, prim_re_RES, prim_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                      FULL_SPAN, strm)
                  end
            in
              (case (lex(strm))
               of (Tok.BAR, _, strm') => post_re_PROD_1_SUBRULE_1_PROD_6(strm)
                | (Tok.AMP, _, strm') => post_re_PROD_1_SUBRULE_1_PROD_6(strm)
                | (Tok.DOT, _, strm') => post_re_PROD_1_SUBRULE_1_PROD_6(strm)
                | (Tok.SEMI, _, strm') => post_re_PROD_1_SUBRULE_1_PROD_6(strm)
                | (Tok.LP, _, strm') => post_re_PROD_1_SUBRULE_1_PROD_6(strm)
                | (Tok.RP, _, strm') => post_re_PROD_1_SUBRULE_1_PROD_6(strm)
                | (Tok.LSB, _, strm') => post_re_PROD_1_SUBRULE_1_PROD_6(strm)
                | (Tok.DARROW, _, strm') =>
                    post_re_PROD_1_SUBRULE_1_PROD_6(strm)
                | (Tok.CHAR(_), _, strm') =>
                    post_re_PROD_1_SUBRULE_1_PROD_6(strm)
                | (Tok.UCHAR(_), _, strm') =>
                    post_re_PROD_1_SUBRULE_1_PROD_6(strm)
                | (Tok.LCB, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.ID(_), _, strm') =>
                          post_re_PROD_1_SUBRULE_1_PROD_6(strm)
                      | (Tok.INT(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.COMMA, _, strm') =>
                                post_re_PROD_1_SUBRULE_1_PROD_5(strm)
                            | (Tok.RCB, _, strm') =>
                                post_re_PROD_1_SUBRULE_1_PROD_4(strm)
                            | _ => raise(ParseError)
                          (* end case *))
                      | _ => raise(ParseError)
                    (* end case *))
                | (Tok.STAR, _, strm') => post_re_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.QUERY, _, strm') =>
                    post_re_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.PLUS, _, strm') => post_re_PROD_1_SUBRULE_1_PROD_3(strm)
                | _ => raise(ParseError)
              (* end case *))
            end
      in
        (wrap post_re_PROD_1_SUBRULE_1_NT)(strm')
      end
      val FULL_SPAN = (#1(prim_re_SPAN), #2(SR_SPAN))
      in
        (UserCode.post_re_PROD_1_ACT (SR_RES, env_RES, prim_re_RES, SR_SPAN : (Lex.pos * Lex.pos), prim_re_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
          FULL_SPAN, strm')
      end
and prim_re_NT (env_RES) (strm) = let
      fun prim_re_PROD_1 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.prim_re_PROD_1_ACT (ID_RES, LCB_RES, RCB_RES, env_RES, ID_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      fun prim_re_PROD_2 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (re_RES, re_SPAN, strm') = (wrap (re_NT (UserCode.ARGS_47 (LP_RES, env_RES, errs_REFC))))(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((re_RES), FULL_SPAN, strm')
            end
      fun prim_re_PROD_3 (strm) = let
            val (char_RES, char_SPAN, strm') = (wrap char_NT)(strm)
            val FULL_SPAN = (#1(char_SPAN), #2(char_SPAN))
            in
              (UserCode.prim_re_PROD_3_ACT (env_RES, char_RES, char_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      fun prim_re_PROD_4 (strm) = let
            val (DOT_RES, DOT_SPAN, strm') = matchDOT(strm)
            val FULL_SPAN = (#1(DOT_SPAN), #2(DOT_SPAN))
            in
              (UserCode.prim_re_PROD_4_ACT (DOT_RES, env_RES, DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      fun prim_re_PROD_5 (strm) = let
            val (LSB_RES, LSB_SPAN, strm') = matchLSB(strm)
            val (SR1_RES, SR1_SPAN, strm') = let
            fun prim_re_PROD_5_SUBRULE_1_NT (strm) = let
                  fun prim_re_PROD_5_SUBRULE_1_PROD_1 (strm) = let
                        val (CARAT_RES, CARAT_SPAN, strm') = matchCARAT(strm)
                        val FULL_SPAN = (#1(CARAT_SPAN), #2(CARAT_SPAN))
                        in
                          (UserCode.prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_1_PROD_1_ACT (LSB_RES, env_RES, CARAT_RES, LSB_SPAN : (Lex.pos * Lex.pos), CARAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                            FULL_SPAN, strm')
                        end
                  fun prim_re_PROD_5_SUBRULE_1_PROD_2 (strm) = let
                        val (DASH_RES, DASH_SPAN, strm') = matchDASH(strm)
                        val FULL_SPAN = (#1(DASH_SPAN), #2(DASH_SPAN))
                        in
                          (UserCode.prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_1_PROD_2_ACT (LSB_RES, env_RES, DASH_RES, LSB_SPAN : (Lex.pos * Lex.pos), DASH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                            FULL_SPAN, strm')
                        end
                  fun prim_re_PROD_5_SUBRULE_1_PROD_3 (strm) = let
                        val FULL_SPAN = (R.getPos(strm), R.getPos(strm))
                        in
                          (UserCode.prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_1_PROD_3_ACT (LSB_RES, env_RES, LSB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                            FULL_SPAN, strm)
                        end
                  in
                    (case (lex(strm))
                     of (Tok.RSB, _, strm') =>
                          prim_re_PROD_5_SUBRULE_1_PROD_3(strm)
                      | (Tok.CHAR(_), _, strm') =>
                          prim_re_PROD_5_SUBRULE_1_PROD_3(strm)
                      | (Tok.UCHAR(_), _, strm') =>
                          prim_re_PROD_5_SUBRULE_1_PROD_3(strm)
                      | (Tok.CARAT, _, strm') =>
                          prim_re_PROD_5_SUBRULE_1_PROD_1(strm)
                      | (Tok.DASH, _, strm') =>
                          tryProds(strm, [prim_re_PROD_5_SUBRULE_1_PROD_2,
                            prim_re_PROD_5_SUBRULE_1_PROD_3])
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            in
              (wrap prim_re_PROD_5_SUBRULE_1_NT)(strm')
            end
            fun prim_re_PROD_5_SUBRULE_2_NT (strm) = let
                  fun prim_re_PROD_5_SUBRULE_2_PROD_1 (strm) = let
                        val (char1_RES, char1_SPAN, strm') = (wrap char_NT)(strm)
                        val (DASH_RES, DASH_SPAN, strm') = matchDASH(strm')
                        val (char2_RES, char2_SPAN, strm') = (wrap char_NT)(strm')
                        val FULL_SPAN = (#1(char1_SPAN), #2(char2_SPAN))
                        in
                          (UserCode.prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_2_PROD_1_ACT (LSB_RES, SR1_RES, env_RES, DASH_RES, char1_RES, char2_RES, LSB_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), DASH_SPAN : (Lex.pos * Lex.pos), char1_SPAN : (Lex.pos * Lex.pos), char2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                            FULL_SPAN, strm')
                        end
                  fun prim_re_PROD_5_SUBRULE_2_PROD_2 (strm) = let
                        val (char_RES, char_SPAN, strm') = (wrap char_NT)(strm)
                        val FULL_SPAN = (#1(char_SPAN), #2(char_SPAN))
                        in
                          (UserCode.prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_2_PROD_2_ACT (LSB_RES, SR1_RES, env_RES, char_RES, LSB_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), char_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.CHAR(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.DASH, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.RSB, _, strm') =>
                                      prim_re_PROD_5_SUBRULE_2_PROD_2(strm)
                                  | (Tok.CHAR(_), _, strm') =>
                                      prim_re_PROD_5_SUBRULE_2_PROD_1(strm)
                                  | (Tok.UCHAR(_), _, strm') =>
                                      prim_re_PROD_5_SUBRULE_2_PROD_1(strm)
                                  | _ => raise(ParseError)
                                (* end case *))
                            | (Tok.RSB, _, strm') =>
                                prim_re_PROD_5_SUBRULE_2_PROD_2(strm)
                            | (Tok.CHAR(_), _, strm') =>
                                prim_re_PROD_5_SUBRULE_2_PROD_2(strm)
                            | (Tok.UCHAR(_), _, strm') =>
                                prim_re_PROD_5_SUBRULE_2_PROD_2(strm)
                            | _ => raise(ParseError)
                          (* end case *))
                      | (Tok.UCHAR(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.DASH, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.RSB, _, strm') =>
                                      prim_re_PROD_5_SUBRULE_2_PROD_2(strm)
                                  | (Tok.CHAR(_), _, strm') =>
                                      prim_re_PROD_5_SUBRULE_2_PROD_1(strm)
                                  | (Tok.UCHAR(_), _, strm') =>
                                      prim_re_PROD_5_SUBRULE_2_PROD_1(strm)
                                  | _ => raise(ParseError)
                                (* end case *))
                            | (Tok.RSB, _, strm') =>
                                prim_re_PROD_5_SUBRULE_2_PROD_2(strm)
                            | (Tok.CHAR(_), _, strm') =>
                                prim_re_PROD_5_SUBRULE_2_PROD_2(strm)
                            | (Tok.UCHAR(_), _, strm') =>
                                prim_re_PROD_5_SUBRULE_2_PROD_2(strm)
                            | _ => raise(ParseError)
                          (* end case *))
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            fun prim_re_PROD_5_SUBRULE_2_PRED (strm) = (case (lex(strm))
                   of (Tok.CHAR(_), _, strm') => true
                    | (Tok.UCHAR(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR2_RES, SR2_SPAN, strm') = EBNF.closure(prim_re_PROD_5_SUBRULE_2_PRED, (wrap prim_re_PROD_5_SUBRULE_2_NT), strm')
            val (SR3_RES, SR3_SPAN, strm') = let
            fun prim_re_PROD_5_SUBRULE_3_NT (strm) = let
                  fun prim_re_PROD_5_SUBRULE_3_PROD_1 (strm) = let
                        val (DASH_RES, DASH_SPAN, strm') = matchDASH(strm)
                        val FULL_SPAN = (#1(DASH_SPAN), #2(DASH_SPAN))
                        in
                          (UserCode.prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_3_PROD_1_ACT (LSB_RES, SR1_RES, SR2_RES, env_RES, DASH_RES, LSB_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), DASH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                            FULL_SPAN, strm')
                        end
                  fun prim_re_PROD_5_SUBRULE_3_PROD_2 (strm) = let
                        val FULL_SPAN = (R.getPos(strm), R.getPos(strm))
                        in
                          (UserCode.prim_re_PROD_5_prim_re_prim_re_PROD_5_SUBRULE_3_PROD_2_ACT (LSB_RES, SR1_RES, SR2_RES, env_RES, LSB_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                            FULL_SPAN, strm)
                        end
                  in
                    (case (lex(strm))
                     of (Tok.RSB, _, strm') =>
                          prim_re_PROD_5_SUBRULE_3_PROD_2(strm)
                      | (Tok.DASH, _, strm') =>
                          prim_re_PROD_5_SUBRULE_3_PROD_1(strm)
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            in
              (wrap prim_re_PROD_5_SUBRULE_3_NT)(strm')
            end
            val (RSB_RES, RSB_SPAN, strm') = matchRSB(strm')
            val FULL_SPAN = (#1(LSB_SPAN), #2(RSB_SPAN))
            in
              (UserCode.prim_re_PROD_5_ACT (LSB_RES, RSB_RES, SR1_RES, SR2_RES, SR3_RES, env_RES, LSB_SPAN : (Lex.pos * Lex.pos), RSB_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), SR3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LSB, _, strm') => prim_re_PROD_5(strm)
          | (Tok.CHAR(_), _, strm') => prim_re_PROD_3(strm)
          | (Tok.UCHAR(_), _, strm') => prim_re_PROD_3(strm)
          | (Tok.LCB, _, strm') => prim_re_PROD_1(strm)
          | (Tok.LP, _, strm') => prim_re_PROD_2(strm)
          | (Tok.DOT, _, strm') => prim_re_PROD_4(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun rule_NT (env_RES) (strm) = let
      fun rule_PROD_1_SUBRULE_1_NT (strm) = let
            val (LT_RES, LT_SPAN, strm') = matchLT(strm)
            fun rule_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  val (ID_RES, ID_SPAN, strm') = matchID(strm)
                  fun rule_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                        val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                        val FULL_SPAN = (#1(COMMA_SPAN), #2(COMMA_SPAN))
                        in
                          ((), FULL_SPAN, strm')
                        end
                  fun rule_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                         of (Tok.COMMA, _, strm') => true
                          | _ => false
                        (* end case *))
                  val (COMMA_RES, COMMA_SPAN, strm') = EBNF.optional(rule_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, (wrap rule_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT), strm')
                  val FULL_SPAN = (#1(ID_SPAN), #2(COMMA_SPAN))
                  in
                    (UserCode.rule_PROD_1_rule_rule_PROD_1_SUBRULE_1_PROD_1_rule_PROD_1_SUBRULE_1_rule_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (ID_RES, LT_RES, env_RES, COMMA_RES, ID_SPAN : (Lex.pos * Lex.pos), LT_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                      FULL_SPAN, strm')
                  end
            fun rule_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.ID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.posclos(rule_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, (wrap rule_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT), strm')
            val (GT_RES, GT_SPAN, strm') = matchGT(strm')
            val FULL_SPAN = (#1(LT_SPAN), #2(GT_SPAN))
            in
              ((SR_RES), FULL_SPAN, strm')
            end
      fun rule_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.LT, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(rule_PROD_1_SUBRULE_1_PRED, (wrap rule_PROD_1_SUBRULE_1_NT), strm)
      val (re_RES, re_SPAN, strm') = (wrap (re_NT (UserCode.ARGS_24 (SR_RES, env_RES, errs_REFC))))(strm')
      val (DARROW_RES, DARROW_SPAN, strm') = matchDARROW(strm')
      val (CODE_RES, CODE_SPAN, strm') = matchCODE(strm')
      val FULL_SPAN = (#1(SR_SPAN), #2(CODE_SPAN))
      in
        (UserCode.rule_PROD_1_ACT (SR_RES, re_RES, env_RES, CODE_RES, DARROW_RES, SR_SPAN : (Lex.pos * Lex.pos), re_SPAN : (Lex.pos * Lex.pos), CODE_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
          FULL_SPAN, strm')
      end
fun directive_NT (conf_RES, env_RES) (strm) = let
      fun directive_PROD_1 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (re_RES, re_SPAN, strm') = (wrap (re_NT (UserCode.ARGS_15 (EQ_RES, ID_RES, env_RES, conf_RES, KW_let_RES, errs_REFC))))(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(re_SPAN))
            in
              (UserCode.directive_PROD_1_ACT (EQ_RES, ID_RES, re_RES, env_RES, conf_RES, KW_let_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), re_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      fun directive_PROD_2 (strm) = let
            val (KW_states_RES, KW_states_SPAN, strm') = matchKW_states(strm)
            fun directive_PROD_2_SUBRULE_1_NT (strm) = let
                  val (ID_RES, ID_SPAN, strm') = matchID(strm)
                  fun directive_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                        val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                        val FULL_SPAN = (#1(COMMA_SPAN), #2(COMMA_SPAN))
                        in
                          ((), FULL_SPAN, strm')
                        end
                  fun directive_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                         of (Tok.COMMA, _, strm') => true
                          | _ => false
                        (* end case *))
                  val (COMMA_RES, COMMA_SPAN, strm') = EBNF.optional(directive_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_PRED, (wrap directive_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_NT), strm')
                  val FULL_SPAN = (#1(ID_SPAN), #2(COMMA_SPAN))
                  in
                    (UserCode.directive_PROD_2_directive_directive_PROD_2_SUBRULE_1_PROD_1_ACT (ID_RES, env_RES, conf_RES, COMMA_RES, KW_states_RES, ID_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), KW_states_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                      FULL_SPAN, strm')
                  end
            fun directive_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.ID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.posclos(directive_PROD_2_SUBRULE_1_PRED, (wrap directive_PROD_2_SUBRULE_1_NT), strm')
            val FULL_SPAN = (#1(KW_states_SPAN), #2(SR_SPAN))
            in
              (UserCode.directive_PROD_2_ACT (SR_RES, env_RES, conf_RES, KW_states_RES, SR_SPAN : (Lex.pos * Lex.pos), KW_states_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      fun directive_PROD_3 (strm) = let
            val (KW_charset_RES, KW_charset_SPAN, strm') = matchKW_charset(strm)
            val (SR_RES, SR_SPAN, strm') = let
            fun directive_PROD_3_SUBRULE_1_NT (strm) = let
                  fun directive_PROD_3_SUBRULE_1_PROD_1 (strm) = let
                        val (UTF8_RES, UTF8_SPAN, strm') = matchUTF8(strm)
                        val FULL_SPAN = (#1(UTF8_SPAN), #2(UTF8_SPAN))
                        in
                          (UserCode.directive_PROD_3_directive_directive_PROD_3_SUBRULE_1_PROD_1_ACT (env_RES, UTF8_RES, conf_RES, KW_charset_RES, UTF8_SPAN : (Lex.pos * Lex.pos), KW_charset_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                            FULL_SPAN, strm')
                        end
                  fun directive_PROD_3_SUBRULE_1_PROD_2 (strm) = let
                        val (ASCII7_RES, ASCII7_SPAN, strm') = matchASCII7(strm)
                        val FULL_SPAN = (#1(ASCII7_SPAN), #2(ASCII7_SPAN))
                        in
                          (UserCode.directive_PROD_3_directive_directive_PROD_3_SUBRULE_1_PROD_2_ACT (env_RES, conf_RES, ASCII7_RES, KW_charset_RES, ASCII7_SPAN : (Lex.pos * Lex.pos), KW_charset_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                            FULL_SPAN, strm')
                        end
                  fun directive_PROD_3_SUBRULE_1_PROD_3 (strm) = let
                        val (ASCII8_RES, ASCII8_SPAN, strm') = matchASCII8(strm)
                        val FULL_SPAN = (#1(ASCII8_SPAN), #2(ASCII8_SPAN))
                        in
                          (UserCode.directive_PROD_3_directive_directive_PROD_3_SUBRULE_1_PROD_3_ACT (env_RES, conf_RES, ASCII8_RES, KW_charset_RES, ASCII8_SPAN : (Lex.pos * Lex.pos), KW_charset_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.ASCII8, _, strm') =>
                          directive_PROD_3_SUBRULE_1_PROD_3(strm)
                      | (Tok.UTF8, _, strm') =>
                          directive_PROD_3_SUBRULE_1_PROD_1(strm)
                      | (Tok.ASCII7, _, strm') =>
                          directive_PROD_3_SUBRULE_1_PROD_2(strm)
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            in
              (wrap directive_PROD_3_SUBRULE_1_NT)(strm')
            end
            val FULL_SPAN = (#1(KW_charset_SPAN), #2(SR_SPAN))
            in
              ((SR_RES), FULL_SPAN, strm')
            end
      fun directive_PROD_4 (strm) = let
            val (KW_name_RES, KW_name_SPAN, strm') = matchKW_name(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val FULL_SPAN = (#1(KW_name_SPAN), #2(ID_SPAN))
            in
              (UserCode.directive_PROD_4_ACT (ID_RES, env_RES, conf_RES, KW_name_RES, ID_SPAN : (Lex.pos * Lex.pos), KW_name_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
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
            val (directive_RES, directive_SPAN, strm') = (wrap (directive_NT (UserCode.ARGS_10 (env_RES, spec_RES, errs_REFC))))(strm)
            val FULL_SPAN = (#1(directive_SPAN), #2(directive_SPAN))
            in
              (UserCode.decl_PROD_1_ACT (env_RES, spec_RES, directive_RES, directive_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      fun decl_PROD_2 (strm) = let
            val (KW_defs_RES, KW_defs_SPAN, strm') = matchKW_defs(strm)
            val (CODE_RES, CODE_SPAN, strm') = matchCODE(strm')
            val FULL_SPAN = (#1(KW_defs_SPAN), #2(CODE_SPAN))
            in
              (UserCode.decl_PROD_2_ACT (env_RES, CODE_RES, spec_RES, KW_defs_RES, CODE_SPAN : (Lex.pos * Lex.pos), KW_defs_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      fun decl_PROD_3 (strm) = let
            val (rule_RES, rule_SPAN, strm') = (wrap (rule_NT (UserCode.ARGS_13 (env_RES, spec_RES, errs_REFC))))(strm)
            val FULL_SPAN = (#1(rule_SPAN), #2(rule_SPAN))
            in
              (UserCode.decl_PROD_3_ACT (env_RES, rule_RES, spec_RES, rule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.DOT, _, strm') => decl_PROD_3(strm)
          | (Tok.LP, _, strm') => decl_PROD_3(strm)
          | (Tok.LSB, _, strm') => decl_PROD_3(strm)
          | (Tok.LCB, _, strm') => decl_PROD_3(strm)
          | (Tok.LT, _, strm') => decl_PROD_3(strm)
          | (Tok.CARAT, _, strm') => decl_PROD_3(strm)
          | (Tok.CHAR(_), _, strm') => decl_PROD_3(strm)
          | (Tok.UCHAR(_), _, strm') => decl_PROD_3(strm)
          | (Tok.KW_name, _, strm') => decl_PROD_1(strm)
          | (Tok.KW_states, _, strm') => decl_PROD_1(strm)
          | (Tok.KW_let, _, strm') => decl_PROD_1(strm)
          | (Tok.KW_charset, _, strm') => decl_PROD_1(strm)
          | (Tok.KW_defs, _, strm') => decl_PROD_2(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun decls_NT (spec_RES, env_RES) (strm) = let
      fun decls_PROD_1 (strm) = let
            val (decl_RES, decl_SPAN, strm') = (wrap (decl_NT (UserCode.ARGS_6 (env_RES, spec_RES, errs_REFC))))(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (decls_RES, decls_SPAN, strm') = (wrap (decls_NT (UserCode.ARGS_7 (env_RES, SEMI_RES, decl_RES, spec_RES, errs_REFC))))(strm')
            val FULL_SPAN = (#1(decl_SPAN), #2(decls_SPAN))
            in
              (UserCode.decls_PROD_1_ACT (env_RES, SEMI_RES, decl_RES, spec_RES, decls_RES, SEMI_SPAN : (Lex.pos * Lex.pos), decl_SPAN : (Lex.pos * Lex.pos), decls_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm')
            end
      fun decls_PROD_2 (strm) = let
            val FULL_SPAN = (R.getPos(strm), R.getPos(strm))
            in
              (UserCode.decls_PROD_2_ACT (env_RES, spec_RES, FULL_SPAN : (Lex.pos * Lex.pos), errs_REFC),
                FULL_SPAN, strm)
            end
      in
        (case (lex(strm))
         of (Tok.EOF, _, strm') => decls_PROD_2(strm)
          | (Tok.DOT, _, strm') => decls_PROD_1(strm)
          | (Tok.LP, _, strm') => decls_PROD_1(strm)
          | (Tok.LSB, _, strm') => decls_PROD_1(strm)
          | (Tok.LCB, _, strm') => decls_PROD_1(strm)
          | (Tok.LT, _, strm') => decls_PROD_1(strm)
          | (Tok.CARAT, _, strm') => decls_PROD_1(strm)
          | (Tok.KW_defs, _, strm') => decls_PROD_1(strm)
          | (Tok.KW_name, _, strm') => decls_PROD_1(strm)
          | (Tok.KW_states, _, strm') => decls_PROD_1(strm)
          | (Tok.KW_let, _, strm') => decls_PROD_1(strm)
          | (Tok.KW_charset, _, strm') => decls_PROD_1(strm)
          | (Tok.CHAR(_), _, strm') => decls_PROD_1(strm)
          | (Tok.UCHAR(_), _, strm') => decls_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun file_NT (strm) = let
      val (decls_RES, decls_SPAN, strm') = (wrap (decls_NT (UserCode.ARGS_4 (errs_REFC))))(strm)
      val FULL_SPAN = (#1(decls_SPAN), #2(decls_SPAN))
      in
        ((decls_RES), FULL_SPAN, strm')
      end
in
  (file_NT)
end
val file_NT =  fn s => unwrap (Err.launch eh (reqEOF (file_NT )) (R.wrap (s, lexFn)))

in (file_NT) end
  in
fun parse lexFn  s = let val (file_NT) = mk lexFn in file_NT s end

  end

end
