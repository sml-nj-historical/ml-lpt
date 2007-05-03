structure 
SpecTokens = struct

    datatype token = EOF
      | BOGUS
      | INT of string
      | TYVAR of string
      | IDDOT of string
      | STRING of string
      | ID of string
      | OF
      | REFSET
      | DARROW
      | ARROW
      | EQ
      | CODE of string
      | QUERY
      | PLUS
      | STAR
      | BAR
      | DOLLAR
      | AT
      | COMMA
      | SEMI
      | COLON
      | SLASH
      | RCB
      | LCB
      | RSB
      | LSB
      | RP
      | LP
      | KW_nonterms
      | KW_dropping
      | KW_where
      | KW_try
      | KW_refcell
      | KW_entry
      | KW_start
      | KW_name
      | KW_import
      | KW_keywords
      | KW_defs
      | KW_tokens

    val allToks = [EOF, BOGUS, OF, REFSET, DARROW, ARROW, EQ, QUERY, PLUS, STAR, BAR, DOLLAR, AT, COMMA, SEMI, COLON, SLASH, RCB, LCB, RSB, LSB, RP, LP, KW_nonterms, KW_dropping, KW_where, KW_try, KW_refcell, KW_entry, KW_start, KW_name, KW_import, KW_keywords, KW_defs, KW_tokens]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (BOGUS) => "BOGUS"
  | (INT(_)) => "INT"
  | (TYVAR(_)) => "TYVAR"
  | (IDDOT(_)) => "IDDOT"
  | (STRING(_)) => "STRING"
  | (ID(_)) => "ID"
  | (OF) => "of"
  | (REFSET) => ":="
  | (DARROW) => "=>"
  | (ARROW) => "->"
  | (EQ) => "="
  | (CODE(_)) => "CODE"
  | (QUERY) => "?"
  | (PLUS) => "+"
  | (STAR) => "*"
  | (BAR) => "|"
  | (DOLLAR) => "$"
  | (AT) => "@"
  | (COMMA) => ","
  | (SEMI) => ";"
  | (COLON) => ":"
  | (SLASH) => "/"
  | (RCB) => "}"
  | (LCB) => "{"
  | (RSB) => "]"
  | (LSB) => "["
  | (RP) => ")"
  | (LP) => "("
  | (KW_nonterms) => "%nonterms"
  | (KW_dropping) => "%dropping"
  | (KW_where) => "%where"
  | (KW_try) => "%try"
  | (KW_refcell) => "%refcell"
  | (KW_entry) => "%entry"
  | (KW_start) => "%start"
  | (KW_name) => "%name"
  | (KW_import) => "%import"
  | (KW_keywords) => "%keywords"
  | (KW_defs) => "%defs"
  | (KW_tokens) => "%tokens"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (BOGUS) => false
  | (INT(_)) => false
  | (TYVAR(_)) => false
  | (IDDOT(_)) => false
  | (STRING(_)) => false
  | (ID(_)) => false
  | (OF) => false
  | (REFSET) => false
  | (DARROW) => false
  | (ARROW) => false
  | (EQ) => false
  | (CODE(_)) => false
  | (QUERY) => false
  | (PLUS) => false
  | (STAR) => false
  | (BAR) => false
  | (DOLLAR) => false
  | (AT) => false
  | (COMMA) => false
  | (SEMI) => false
  | (COLON) => false
  | (SLASH) => false
  | (RCB) => false
  | (LCB) => false
  | (RSB) => false
  | (LSB) => false
  | (RP) => false
  | (LP) => false
  | (KW_nonterms) => false
  | (KW_dropping) => false
  | (KW_where) => false
  | (KW_try) => false
  | (KW_refcell) => false
  | (KW_entry) => false
  | (KW_start) => false
  | (KW_name) => false
  | (KW_import) => false
  | (KW_keywords) => false
  | (KW_defs) => false
  | (KW_tokens) => false
(* end case *))


  fun toksToString toks = String.concatWith " " (map toString toks)

  fun isEOF EOF = true
    | isEOF _ = false

end

functor SpecParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
SpecTokens
    structure UserCode = struct

 
  structure GS = GrammarSyntax

  fun lift f (vspan, v) = (vspan, f v)

  val trimQuotes = 
        Substring.string o
	(Substring.triml 1) o
	(Substring.trimr 1) o 
	Substring.full



fun File_PROD_1_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( List.concat SR) : GS.grammar
fun Decl_PROD_1_ACT (ID, KW_name, ID_SPAN : (Lex.pos * Lex.pos), KW_name_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [ (FULL_SPAN, GS.NAME ID) ]) : (GS.span * GS.decl) list
fun Decl_PROD_2_ACT (ID, KW_start, ID_SPAN : (Lex.pos * Lex.pos), KW_start_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [ (FULL_SPAN, GS.START (Atom.atom ID)) ]) : (GS.span * GS.decl) list
fun Decl_PROD_3_ACT (KW_entry, IDList, KW_entry_SPAN : (Lex.pos * Lex.pos), IDList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( map (lift (GS.ENTRY o Atom.atom)) IDList) : (GS.span * GS.decl) list
fun Decl_PROD_4_ACT (KW_keywords, SymList, KW_keywords_SPAN : (Lex.pos * Lex.pos), SymList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( map (lift GS.KEYWORD) SymList) : (GS.span * GS.decl) list
fun Decl_PROD_5_ACT (Code, KW_defs, Code_SPAN : (Lex.pos * Lex.pos), KW_defs_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [ (FULL_SPAN, GS.DEFS Code) ]) : (GS.span * GS.decl) list
fun Decl_PROD_6_ACT (COLON, KW_tokens, ConstrList, COLON_SPAN : (Lex.pos * Lex.pos), KW_tokens_SPAN : (Lex.pos * Lex.pos), ConstrList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( map (lift GS.TOKEN) ConstrList) : (GS.span * GS.decl) list
fun Decl_PROD_7_ACT (SR, STRING, KW_import, SR_SPAN : (Lex.pos * Lex.pos), STRING_SPAN : (Lex.pos * Lex.pos), KW_import_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [ (FULL_SPAN, GS.IMPORT {
	      filename = trimQuotes STRING,
	      dropping = getOpt (SR, [])
	    }) ]) : (GS.span * GS.decl) list
fun Decl_PROD_8_ACT (EQ, ID, Ty, Code, COLON, KW_refcell, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), Ty_SPAN : (Lex.pos * Lex.pos), Code_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), KW_refcell_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [ (FULL_SPAN, GS.REFCELL (ID, Ty, Code)) ]) : (GS.span * GS.decl) list
fun Decl_PROD_9_ACT (SR, COLON, TyAnn, KW_nonterms, SR_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), TyAnn_SPAN : (Lex.pos * Lex.pos), KW_nonterms_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( map (lift GS.NONTERM) (TyAnn::SR)) : (GS.span * GS.decl) list
fun Decl_PROD_10_ACT (ID, COLON, AltList, Formals, ID_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), AltList_SPAN : (Lex.pos * Lex.pos), Formals_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( map (fn (span, alt) => 
	    (span, GS.RULE {
	      lhs = Atom.atom ID,
	      formals = getOpt(Formals, []),
	      rhs = alt
	    })) AltList) : (GS.span * GS.decl) list
fun Formals_PROD_1_ACT (ID, LP, RP, SR, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ID::SR)
fun IDList_PROD_1_ACT (SR, ID', SR_SPAN : (Lex.pos * Lex.pos), ID'_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ID'::SR)
fun SymList_PROD_1_ACT (SR, Symbol, SR_SPAN : (Lex.pos * Lex.pos), Symbol_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Symbol::SR)
fun AltList_PROD_1_ACT (SR, Alt, SR_SPAN : (Lex.pos * Lex.pos), Alt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Alt::SR)
fun TyAnn_PROD_1_TyAnn_TyAnn_PROD_1_SUBRULE_1_PROD_2_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "unit")
fun TyAnn_PROD_1_ACT (ID, SR, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( FULL_SPAN, (Atom.atom ID, SR))
fun Alt_PROD_1_ACT (SR1, SR2, NamedItem, KW_try, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), NamedItem_SPAN : (Lex.pos * Lex.pos), KW_try_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( FULL_SPAN, GS.RHS {
        items = NamedItem,
	try = isSome KW_try,
	predicate = SR1,
	action = SR2,
	loc = FULL_SPAN
      })
fun NamedItem_PROD_1_ACT (SR, Item, SR_SPAN : (Lex.pos * Lex.pos), Item_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( SR, Item)
fun Item_PROD_1_Item_Item_PROD_1_SUBRULE_1_PROD_1_ACT (STAR, PrimItem, STAR_SPAN : (Lex.pos * Lex.pos), PrimItem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PrimItem_SPAN, GS.CLOS PrimItem)
fun Item_PROD_1_Item_Item_PROD_1_SUBRULE_1_PROD_2_ACT (PLUS, PrimItem, PLUS_SPAN : (Lex.pos * Lex.pos), PrimItem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PrimItem_SPAN, GS.POSCLOS PrimItem)
fun Item_PROD_1_Item_Item_PROD_1_SUBRULE_1_PROD_3_ACT (QUERY, PrimItem, QUERY_SPAN : (Lex.pos * Lex.pos), PrimItem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PrimItem_SPAN, GS.OPT PrimItem)
fun Item_PROD_1_Item_Item_PROD_1_SUBRULE_1_PROD_4_ACT (PrimItem, PrimItem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PrimItem)
fun Item_PROD_1_ACT (SR, PrimItem, SR_SPAN : (Lex.pos * Lex.pos), PrimItem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( SR)
fun PrimItem_PROD_1_ACT (SR, Symbol, SR_SPAN : (Lex.pos * Lex.pos), Symbol_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( FULL_SPAN, GS.SYMBOL ((fn (_, s) => s) Symbol, SR))
fun PrimItem_PROD_2_ACT (LP, RP, AltList, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), AltList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( FULL_SPAN, GS.SUBRULE (map (fn (_, alt) => alt) AltList))
fun ID'_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ID_SPAN, ID)
fun Symbol_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ID_SPAN, Atom.atom ID)
fun Symbol_PROD_2_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( STRING_SPAN, Atom.atom STRING)
fun Constr_PROD_1_ACT (ID, SR, Abbrev, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Abbrev_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( FULL_SPAN, (Atom.atom ID, SR, Abbrev))
fun ConstrList_PROD_1_ACT (SR, Constr, SR_SPAN : (Lex.pos * Lex.pos), Constr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Constr::SR)
fun Ty_PROD_1_ACT (LCB, RCB, RowList, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), RowList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "{ " ^ RowList ^" } ")
fun Ty_PROD_2_ACT (LCB, RCB, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "{}")
fun Row_PROD_1_ACT (Ty, COLON, Label, Ty_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), Label_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Label ^ " : " ^ Ty)
fun RowList_PROD_1_ACT (SR, Row, SR_SPAN : (Lex.pos * Lex.pos), Row_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( String.concatWith "," (Row::SR))
fun TyApp_PROD_1_ACT (LP, RP, SR, Ty, QualID, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Ty_SPAN : (Lex.pos * Lex.pos), QualID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "(" ^ String.concatWith "," (Ty::SR) ^ ") " ^ QualID)
fun TyApp_PROD_2_TyApp_TyApp_PROD_2_SUBRULE_1_PROD_1_ACT (QualID, TyProd, QualID_SPAN : (Lex.pos * Lex.pos), TyProd_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( TyProd ^ " " ^ QualID)
fun TyApp_PROD_2_TyApp_TyApp_PROD_2_SUBRULE_1_PROD_2_ACT (TyProd, TyProd_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( TyProd)
fun TyApp_PROD_2_ACT (SR, TyProd, SR_SPAN : (Lex.pos * Lex.pos), TyProd_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( SR)
fun TyProd_PROD_1_ACT (SR, TyFun, SR_SPAN : (Lex.pos * Lex.pos), TyFun_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( String.concatWith " * " (TyFun::SR))
fun TyFun_PROD_1_ACT (SR, TyAtom, SR_SPAN : (Lex.pos * Lex.pos), TyAtom_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( String.concatWith " -> " (TyAtom::SR))
fun TyAtom_PROD_2_ACT (LP, RP, Ty, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Ty_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "(" ^ Ty ^ ")")
fun QualID_PROD_2_ACT (QualID, IDDOT, QualID_SPAN : (Lex.pos * Lex.pos), IDDOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( IDDOT ^ QualID)
fun Abbrev_PROD_1_ACT (LP, RP, STRING, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Atom.atom STRING)
fun Code_PROD_1_ACT (CODE, CODE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( FULL_SPAN, CODE)

    end

    structure R = RepairableStrm(Tok)(Lex)
    structure Err = ErrHandler(R)
    structure EBNF = EBNF(R)

    exception ParseError = Err.RepairableError

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, R.unwrap strm, repairs)
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
val matchINT = wrap (fn strm => (case (lex(strm))
 of (Tok.INT(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchTYVAR = wrap (fn strm => (case (lex(strm))
 of (Tok.TYVAR(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchIDDOT = wrap (fn strm => (case (lex(strm))
 of (Tok.IDDOT(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSTRING = wrap (fn strm => (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchID = wrap (fn strm => (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchOF = wrap (fn strm => (case (lex(strm))
 of (Tok.OF, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchREFSET = wrap (fn strm => (case (lex(strm))
 of (Tok.REFSET, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDARROW = wrap (fn strm => (case (lex(strm))
 of (Tok.DARROW, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchARROW = wrap (fn strm => (case (lex(strm))
 of (Tok.ARROW, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchEQ = wrap (fn strm => (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCODE = wrap (fn strm => (case (lex(strm))
 of (Tok.CODE(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchQUERY = wrap (fn strm => (case (lex(strm))
 of (Tok.QUERY, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchPLUS = wrap (fn strm => (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSTAR = wrap (fn strm => (case (lex(strm))
 of (Tok.STAR, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchBAR = wrap (fn strm => (case (lex(strm))
 of (Tok.BAR, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDOLLAR = wrap (fn strm => (case (lex(strm))
 of (Tok.DOLLAR, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchAT = wrap (fn strm => (case (lex(strm))
 of (Tok.AT, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCOMMA = wrap (fn strm => (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSEMI = wrap (fn strm => (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCOLON = wrap (fn strm => (case (lex(strm))
 of (Tok.COLON, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSLASH = wrap (fn strm => (case (lex(strm))
 of (Tok.SLASH, span, strm') => ((), span, strm')
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
val matchKW_nonterms = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_nonterms, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_dropping = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_dropping, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_where = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_where, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_try = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_try, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_refcell = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_refcell, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_entry = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_entry, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_start = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_start, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_name = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_name, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_import = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_import, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_keywords = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_keywords, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_defs = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_defs, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_tokens = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_tokens, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))

val (File_NT) = 
let
fun Code_NT (strm) = let
      val (CODE_RES, CODE_SPAN, strm') = matchCODE(strm)
      val FULL_SPAN = (#1(CODE_SPAN), #2(CODE_SPAN))
      in
        (UserCode.Code_PROD_1_ACT (CODE_RES, CODE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Symbol_NT (strm) = let
      fun Symbol_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.Symbol_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Symbol_PROD_2 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.Symbol_PROD_2_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.STRING(_), _, strm') => Symbol_PROD_2(strm)
          | (Tok.ID(_), _, strm') => Symbol_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun AltList_NT (strm) = let
      val (Alt_RES, Alt_SPAN, strm') = (wrap Alt_NT)(strm)
      fun AltList_PROD_1_SUBRULE_1_NT (strm) = let
            val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
            val (Alt_RES, Alt_SPAN, strm') = (wrap Alt_NT)(strm')
            val FULL_SPAN = (#1(BAR_SPAN), #2(Alt_SPAN))
            in
              ((Alt_RES), FULL_SPAN, strm')
            end
      fun AltList_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.BAR, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(AltList_PROD_1_SUBRULE_1_PRED, (wrap AltList_PROD_1_SUBRULE_1_NT), strm')
      val FULL_SPAN = (#1(Alt_SPAN), #2(SR_SPAN))
      in
        (UserCode.AltList_PROD_1_ACT (SR_RES, Alt_RES, SR_SPAN : (Lex.pos * Lex.pos), Alt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and Alt_NT (strm) = let
      fun Alt_PROD_1_SUBRULE_1_NT (strm) = let
            val (KW_try_RES, KW_try_SPAN, strm') = matchKW_try(strm)
            val FULL_SPAN = (#1(KW_try_SPAN), #2(KW_try_SPAN))
            in
              ((), FULL_SPAN, strm')
            end
      fun Alt_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_try, _, strm') => true
              | _ => false
            (* end case *))
      val (KW_try_RES, KW_try_SPAN, strm') = EBNF.optional(Alt_PROD_1_SUBRULE_1_PRED, (wrap Alt_PROD_1_SUBRULE_1_NT), strm)
      fun Alt_PROD_1_SUBRULE_2_NT (strm) = let
            val (NamedItem_RES, NamedItem_SPAN, strm') = (wrap NamedItem_NT)(strm)
            val FULL_SPAN = (#1(NamedItem_SPAN), #2(NamedItem_SPAN))
            in
              ((NamedItem_RES), FULL_SPAN, strm')
            end
      fun Alt_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.LP, _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | (Tok.STRING(_), _, strm') => true
              | _ => false
            (* end case *))
      val (NamedItem_RES, NamedItem_SPAN, strm') = EBNF.closure(Alt_PROD_1_SUBRULE_2_PRED, (wrap Alt_PROD_1_SUBRULE_2_NT), strm')
      fun Alt_PROD_1_SUBRULE_3_NT (strm) = let
            val (KW_where_RES, KW_where_SPAN, strm') = matchKW_where(strm)
            val (Code_RES, Code_SPAN, strm') = (wrap Code_NT)(strm')
            val FULL_SPAN = (#1(KW_where_SPAN), #2(Code_SPAN))
            in
              ((Code_RES), FULL_SPAN, strm')
            end
      fun Alt_PROD_1_SUBRULE_3_PRED (strm) = (case (lex(strm))
             of (Tok.KW_where, _, strm') => true
              | _ => false
            (* end case *))
      val (SR1_RES, SR1_SPAN, strm') = EBNF.optional(Alt_PROD_1_SUBRULE_3_PRED, (wrap Alt_PROD_1_SUBRULE_3_NT), strm')
      fun Alt_PROD_1_SUBRULE_4_NT (strm) = let
            val (DARROW_RES, DARROW_SPAN, strm') = matchDARROW(strm)
            val (Code_RES, Code_SPAN, strm') = (wrap Code_NT)(strm')
            val FULL_SPAN = (#1(DARROW_SPAN), #2(Code_SPAN))
            in
              ((Code_RES), FULL_SPAN, strm')
            end
      fun Alt_PROD_1_SUBRULE_4_PRED (strm) = (case (lex(strm))
             of (Tok.DARROW, _, strm') => true
              | _ => false
            (* end case *))
      val (SR2_RES, SR2_SPAN, strm') = EBNF.optional(Alt_PROD_1_SUBRULE_4_PRED, (wrap Alt_PROD_1_SUBRULE_4_NT), strm')
      val FULL_SPAN = (#1(KW_try_SPAN), #2(SR2_SPAN))
      in
        (UserCode.Alt_PROD_1_ACT (SR1_RES, SR2_RES, NamedItem_RES, KW_try_RES, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), NamedItem_SPAN : (Lex.pos * Lex.pos), KW_try_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and NamedItem_NT (strm) = let
      fun NamedItem_PROD_1_SUBRULE_1_NT (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(EQ_SPAN))
            in
              ((ID_RES), FULL_SPAN, strm')
            end
      fun NamedItem_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') =>
                  (case (lex(strm'))
                   of (Tok.EQ, _, strm') => true
                    | _ => false
                  (* end case *))
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(NamedItem_PROD_1_SUBRULE_1_PRED, (wrap NamedItem_PROD_1_SUBRULE_1_NT), strm)
      val (Item_RES, Item_SPAN, strm') = (wrap Item_NT)(strm')
      val FULL_SPAN = (#1(SR_SPAN), #2(Item_SPAN))
      in
        (UserCode.NamedItem_PROD_1_ACT (SR_RES, Item_RES, SR_SPAN : (Lex.pos * Lex.pos), Item_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and Item_NT (strm) = let
      val (PrimItem_RES, PrimItem_SPAN, strm') = (wrap PrimItem_NT)(strm)
      val (SR_RES, SR_SPAN, strm') = let
      fun Item_PROD_1_SUBRULE_1_NT (strm) = let
            fun Item_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (STAR_RES, STAR_SPAN, strm') = matchSTAR(strm)
                  val FULL_SPAN = (#1(STAR_SPAN), #2(STAR_SPAN))
                  in
                    (UserCode.Item_PROD_1_Item_Item_PROD_1_SUBRULE_1_PROD_1_ACT (STAR_RES, PrimItem_RES, STAR_SPAN : (Lex.pos * Lex.pos), PrimItem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun Item_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
                  val FULL_SPAN = (#1(PLUS_SPAN), #2(PLUS_SPAN))
                  in
                    (UserCode.Item_PROD_1_Item_Item_PROD_1_SUBRULE_1_PROD_2_ACT (PLUS_RES, PrimItem_RES, PLUS_SPAN : (Lex.pos * Lex.pos), PrimItem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun Item_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                  val (QUERY_RES, QUERY_SPAN, strm') = matchQUERY(strm)
                  val FULL_SPAN = (#1(QUERY_SPAN), #2(QUERY_SPAN))
                  in
                    (UserCode.Item_PROD_1_Item_Item_PROD_1_SUBRULE_1_PROD_3_ACT (QUERY_RES, PrimItem_RES, QUERY_SPAN : (Lex.pos * Lex.pos), PrimItem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun Item_PROD_1_SUBRULE_1_PROD_4 (strm) = let
                  val FULL_SPAN = (R.getPos(strm), R.getPos(strm))
                  in
                    (UserCode.Item_PROD_1_Item_Item_PROD_1_SUBRULE_1_PROD_4_ACT (PrimItem_RES, PrimItem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm)
                  end
            in
              (case (lex(strm))
               of (Tok.KW_where, _, strm') =>
                    Item_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.LP, _, strm') => Item_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.RP, _, strm') => Item_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.SEMI, _, strm') => Item_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.BAR, _, strm') => Item_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.DARROW, _, strm') => Item_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.ID(_), _, strm') => Item_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.STRING(_), _, strm') =>
                    Item_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.PLUS, _, strm') => Item_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.STAR, _, strm') => Item_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.QUERY, _, strm') => Item_PROD_1_SUBRULE_1_PROD_3(strm)
                | _ => raise(ParseError)
              (* end case *))
            end
      in
        (wrap Item_PROD_1_SUBRULE_1_NT)(strm')
      end
      val FULL_SPAN = (#1(PrimItem_SPAN), #2(SR_SPAN))
      in
        (UserCode.Item_PROD_1_ACT (SR_RES, PrimItem_RES, SR_SPAN : (Lex.pos * Lex.pos), PrimItem_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and PrimItem_NT (strm) = let
      fun PrimItem_PROD_1 (strm) = let
            val (Symbol_RES, Symbol_SPAN, strm') = (wrap Symbol_NT)(strm)
            fun PrimItem_PROD_1_SUBRULE_1_NT (strm) = let
                  val (AT_RES, AT_SPAN, strm') = matchAT(strm)
                  val (Code_RES, Code_SPAN, strm') = (wrap Code_NT)(strm')
                  val FULL_SPAN = (#1(AT_SPAN), #2(Code_SPAN))
                  in
                    ((Code_RES), FULL_SPAN, strm')
                  end
            fun PrimItem_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.AT, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(PrimItem_PROD_1_SUBRULE_1_PRED, (wrap PrimItem_PROD_1_SUBRULE_1_NT), strm')
            val FULL_SPAN = (#1(Symbol_SPAN), #2(SR_SPAN))
            in
              (UserCode.PrimItem_PROD_1_ACT (SR_RES, Symbol_RES, SR_SPAN : (Lex.pos * Lex.pos), Symbol_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun PrimItem_PROD_2 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (AltList_RES, AltList_SPAN, strm') = (wrap AltList_NT)(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.PrimItem_PROD_2_ACT (LP_RES, RP_RES, AltList_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), AltList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => PrimItem_PROD_2(strm)
          | (Tok.ID(_), _, strm') => PrimItem_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => PrimItem_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun Formals_NT (strm) = let
      val (LP_RES, LP_SPAN, strm') = matchLP(strm)
      val (ID_RES, ID_SPAN, strm') = matchID(strm')
      fun Formals_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(ID_SPAN))
            in
              ((ID_RES), FULL_SPAN, strm')
            end
      fun Formals_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Formals_PROD_1_SUBRULE_1_PRED, (wrap Formals_PROD_1_SUBRULE_1_NT), strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
      in
        (UserCode.Formals_PROD_1_ACT (ID_RES, LP_RES, RP_RES, SR_RES, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun QualID_NT (strm) = let
      fun QualID_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              ((ID_RES), FULL_SPAN, strm')
            end
      fun QualID_PROD_2 (strm) = let
            val (IDDOT_RES, IDDOT_SPAN, strm') = matchIDDOT(strm)
            val (QualID_RES, QualID_SPAN, strm') = (wrap QualID_NT)(strm')
            val FULL_SPAN = (#1(IDDOT_SPAN), #2(QualID_SPAN))
            in
              (UserCode.QualID_PROD_2_ACT (QualID_RES, IDDOT_RES, QualID_SPAN : (Lex.pos * Lex.pos), IDDOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.IDDOT(_), _, strm') => QualID_PROD_2(strm)
          | (Tok.ID(_), _, strm') => QualID_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun Label_NT (strm) = let
      fun Label_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              ((ID_RES), FULL_SPAN, strm')
            end
      fun Label_PROD_2 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
            in
              ((INT_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.INT(_), _, strm') => Label_PROD_2(strm)
          | (Tok.ID(_), _, strm') => Label_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun Ty_NT (strm) = let
      fun Ty_PROD_1 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (RowList_RES, RowList_SPAN, strm') = (wrap RowList_NT)(strm')
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.Ty_PROD_1_ACT (LCB_RES, RCB_RES, RowList_RES, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), RowList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Ty_PROD_2 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.Ty_PROD_2_ACT (LCB_RES, RCB_RES, LCB_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Ty_PROD_3 (strm) = let
            val (TyApp_RES, TyApp_SPAN, strm') = (wrap TyApp_NT)(strm)
            val FULL_SPAN = (#1(TyApp_SPAN), #2(TyApp_SPAN))
            in
              ((TyApp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => Ty_PROD_3(strm)
          | (Tok.ID(_), _, strm') => Ty_PROD_3(strm)
          | (Tok.IDDOT(_), _, strm') => Ty_PROD_3(strm)
          | (Tok.TYVAR(_), _, strm') => Ty_PROD_3(strm)
          | (Tok.LCB, _, strm') =>
              (case (lex(strm'))
               of (Tok.ID(_), _, strm') => Ty_PROD_1(strm)
                | (Tok.INT(_), _, strm') => Ty_PROD_1(strm)
                | (Tok.RCB, _, strm') => Ty_PROD_2(strm)
                | _ => raise(ParseError)
              (* end case *))
          | _ => raise(ParseError)
        (* end case *))
      end
and TyApp_NT (strm) = let
      fun TyApp_PROD_1 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (Ty_RES, Ty_SPAN, strm') = (wrap Ty_NT)(strm')
            fun TyApp_PROD_1_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Ty_RES, Ty_SPAN, strm') = (wrap Ty_NT)(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Ty_SPAN))
                  in
                    ((Ty_RES), FULL_SPAN, strm')
                  end
            fun TyApp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(TyApp_PROD_1_SUBRULE_1_PRED, (wrap TyApp_PROD_1_SUBRULE_1_NT), strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (QualID_RES, QualID_SPAN, strm') = (wrap QualID_NT)(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(QualID_SPAN))
            in
              (UserCode.TyApp_PROD_1_ACT (LP_RES, RP_RES, SR_RES, Ty_RES, QualID_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Ty_SPAN : (Lex.pos * Lex.pos), QualID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TyApp_PROD_2 (strm) = let
            val (TyProd_RES, TyProd_SPAN, strm') = (wrap TyProd_NT)(strm)
            val (SR_RES, SR_SPAN, strm') = let
            fun TyApp_PROD_2_SUBRULE_1_NT (strm) = let
                  fun TyApp_PROD_2_SUBRULE_1_PROD_1 (strm) = let
                        val (QualID_RES, QualID_SPAN, strm') = (wrap QualID_NT)(strm)
                        val FULL_SPAN = (#1(QualID_SPAN), #2(QualID_SPAN))
                        in
                          (UserCode.TyApp_PROD_2_TyApp_TyApp_PROD_2_SUBRULE_1_PROD_1_ACT (QualID_RES, TyProd_RES, QualID_SPAN : (Lex.pos * Lex.pos), TyProd_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun TyApp_PROD_2_SUBRULE_1_PROD_2 (strm) = let
                        val FULL_SPAN = (R.getPos(strm), R.getPos(strm))
                        in
                          (UserCode.TyApp_PROD_2_TyApp_TyApp_PROD_2_SUBRULE_1_PROD_2_ACT (TyProd_RES, TyProd_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm)
                        end
                  in
                    (case (lex(strm))
                     of (Tok.LP, _, strm') =>
                          TyApp_PROD_2_SUBRULE_1_PROD_2(strm)
                      | (Tok.RP, _, strm') =>
                          TyApp_PROD_2_SUBRULE_1_PROD_2(strm)
                      | (Tok.RCB, _, strm') =>
                          TyApp_PROD_2_SUBRULE_1_PROD_2(strm)
                      | (Tok.SEMI, _, strm') =>
                          TyApp_PROD_2_SUBRULE_1_PROD_2(strm)
                      | (Tok.COMMA, _, strm') =>
                          TyApp_PROD_2_SUBRULE_1_PROD_2(strm)
                      | (Tok.BAR, _, strm') =>
                          TyApp_PROD_2_SUBRULE_1_PROD_2(strm)
                      | (Tok.EQ, _, strm') =>
                          TyApp_PROD_2_SUBRULE_1_PROD_2(strm)
                      | (Tok.ID(_), _, strm') =>
                          TyApp_PROD_2_SUBRULE_1_PROD_1(strm)
                      | (Tok.IDDOT(_), _, strm') =>
                          TyApp_PROD_2_SUBRULE_1_PROD_1(strm)
                      | _ => raise(ParseError)
                    (* end case *))
                  end
            in
              (wrap TyApp_PROD_2_SUBRULE_1_NT)(strm')
            end
            val FULL_SPAN = (#1(TyProd_SPAN), #2(SR_SPAN))
            in
              (UserCode.TyApp_PROD_2_ACT (SR_RES, TyProd_RES, SR_SPAN : (Lex.pos * Lex.pos), TyProd_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => TyApp_PROD_2(strm)
          | (Tok.IDDOT(_), _, strm') => TyApp_PROD_2(strm)
          | (Tok.TYVAR(_), _, strm') => TyApp_PROD_2(strm)
          | (Tok.LP, _, strm') => tryProds(strm, [TyApp_PROD_1, TyApp_PROD_2])
          | _ => raise(ParseError)
        (* end case *))
      end
and TyProd_NT (strm) = let
      val (TyFun_RES, TyFun_SPAN, strm') = (wrap TyFun_NT)(strm)
      fun TyProd_PROD_1_SUBRULE_1_NT (strm) = let
            val (STAR_RES, STAR_SPAN, strm') = matchSTAR(strm)
            val (TyFun_RES, TyFun_SPAN, strm') = (wrap TyFun_NT)(strm')
            val FULL_SPAN = (#1(STAR_SPAN), #2(TyFun_SPAN))
            in
              ((TyFun_RES), FULL_SPAN, strm')
            end
      fun TyProd_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.STAR, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(TyProd_PROD_1_SUBRULE_1_PRED, (wrap TyProd_PROD_1_SUBRULE_1_NT), strm')
      val FULL_SPAN = (#1(TyFun_SPAN), #2(SR_SPAN))
      in
        (UserCode.TyProd_PROD_1_ACT (SR_RES, TyFun_RES, SR_SPAN : (Lex.pos * Lex.pos), TyFun_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and TyFun_NT (strm) = let
      val (TyAtom_RES, TyAtom_SPAN, strm') = (wrap TyAtom_NT)(strm)
      fun TyFun_PROD_1_SUBRULE_1_NT (strm) = let
            val (ARROW_RES, ARROW_SPAN, strm') = matchARROW(strm)
            val (TyAtom_RES, TyAtom_SPAN, strm') = (wrap TyAtom_NT)(strm')
            val FULL_SPAN = (#1(ARROW_SPAN), #2(TyAtom_SPAN))
            in
              ((TyAtom_RES), FULL_SPAN, strm')
            end
      fun TyFun_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ARROW, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(TyFun_PROD_1_SUBRULE_1_PRED, (wrap TyFun_PROD_1_SUBRULE_1_NT), strm')
      val FULL_SPAN = (#1(TyAtom_SPAN), #2(SR_SPAN))
      in
        (UserCode.TyFun_PROD_1_ACT (SR_RES, TyAtom_RES, SR_SPAN : (Lex.pos * Lex.pos), TyAtom_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and TyAtom_NT (strm) = let
      fun TyAtom_PROD_1 (strm) = let
            val (TYVAR_RES, TYVAR_SPAN, strm') = matchTYVAR(strm)
            val FULL_SPAN = (#1(TYVAR_SPAN), #2(TYVAR_SPAN))
            in
              ((TYVAR_RES), FULL_SPAN, strm')
            end
      fun TyAtom_PROD_2 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (Ty_RES, Ty_SPAN, strm') = (wrap Ty_NT)(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.TyAtom_PROD_2_ACT (LP_RES, RP_RES, Ty_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Ty_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TyAtom_PROD_3 (strm) = let
            val (QualID_RES, QualID_SPAN, strm') = (wrap QualID_NT)(strm)
            val FULL_SPAN = (#1(QualID_SPAN), #2(QualID_SPAN))
            in
              ((QualID_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => TyAtom_PROD_3(strm)
          | (Tok.IDDOT(_), _, strm') => TyAtom_PROD_3(strm)
          | (Tok.TYVAR(_), _, strm') => TyAtom_PROD_1(strm)
          | (Tok.LP, _, strm') => TyAtom_PROD_2(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
and RowList_NT (strm) = let
      val (Row_RES, Row_SPAN, strm') = (wrap Row_NT)(strm)
      fun RowList_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (Row_RES, Row_SPAN, strm') = (wrap Row_NT)(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(Row_SPAN))
            in
              ((Row_RES), FULL_SPAN, strm')
            end
      fun RowList_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(RowList_PROD_1_SUBRULE_1_PRED, (wrap RowList_PROD_1_SUBRULE_1_NT), strm')
      val FULL_SPAN = (#1(Row_SPAN), #2(SR_SPAN))
      in
        (UserCode.RowList_PROD_1_ACT (SR_RES, Row_RES, SR_SPAN : (Lex.pos * Lex.pos), Row_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and Row_NT (strm) = let
      val (Label_RES, Label_SPAN, strm') = (wrap Label_NT)(strm)
      val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm')
      val (Ty_RES, Ty_SPAN, strm') = (wrap Ty_NT)(strm')
      val FULL_SPAN = (#1(Label_SPAN), #2(Ty_SPAN))
      in
        (UserCode.Row_PROD_1_ACT (Ty_RES, COLON_RES, Label_RES, Ty_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), Label_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun TyAnn_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val (SR_RES, SR_SPAN, strm') = let
      fun TyAnn_PROD_1_SUBRULE_1_NT (strm) = let
            fun TyAnn_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (OF_RES, OF_SPAN, strm') = matchOF(strm)
                  val (Ty_RES, Ty_SPAN, strm') = (wrap Ty_NT)(strm')
                  val FULL_SPAN = (#1(OF_SPAN), #2(Ty_SPAN))
                  in
                    ((Ty_RES), FULL_SPAN, strm')
                  end
            fun TyAnn_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val FULL_SPAN = (R.getPos(strm), R.getPos(strm))
                  in
                    (UserCode.TyAnn_PROD_1_TyAnn_TyAnn_PROD_1_SUBRULE_1_PROD_2_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm)
                  end
            in
              (case (lex(strm))
               of (Tok.SEMI, _, strm') => TyAnn_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.BAR, _, strm') => TyAnn_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.OF, _, strm') => TyAnn_PROD_1_SUBRULE_1_PROD_1(strm)
                | _ => raise(ParseError)
              (* end case *))
            end
      in
        (wrap TyAnn_PROD_1_SUBRULE_1_NT)(strm')
      end
      val FULL_SPAN = (#1(ID_SPAN), #2(SR_SPAN))
      in
        (UserCode.TyAnn_PROD_1_ACT (ID_RES, SR_RES, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun SymList_NT (strm) = let
      val (Symbol_RES, Symbol_SPAN, strm') = (wrap Symbol_NT)(strm)
      fun SymList_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (Symbol_RES, Symbol_SPAN, strm') = (wrap Symbol_NT)(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(Symbol_SPAN))
            in
              ((Symbol_RES), FULL_SPAN, strm')
            end
      fun SymList_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SymList_PROD_1_SUBRULE_1_PRED, (wrap SymList_PROD_1_SUBRULE_1_NT), strm')
      val FULL_SPAN = (#1(Symbol_SPAN), #2(SR_SPAN))
      in
        (UserCode.SymList_PROD_1_ACT (SR_RES, Symbol_RES, SR_SPAN : (Lex.pos * Lex.pos), Symbol_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Abbrev_NT (strm) = let
      val (LP_RES, LP_SPAN, strm') = matchLP(strm)
      val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
      in
        (UserCode.Abbrev_PROD_1_ACT (LP_RES, RP_RES, STRING_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Constr_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      fun Constr_PROD_1_SUBRULE_1_NT (strm) = let
            val (OF_RES, OF_SPAN, strm') = matchOF(strm)
            val (Ty_RES, Ty_SPAN, strm') = (wrap Ty_NT)(strm')
            val FULL_SPAN = (#1(OF_SPAN), #2(Ty_SPAN))
            in
              ((Ty_RES), FULL_SPAN, strm')
            end
      fun Constr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.OF, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(Constr_PROD_1_SUBRULE_1_PRED, (wrap Constr_PROD_1_SUBRULE_1_NT), strm')
      fun Constr_PROD_1_SUBRULE_2_NT (strm) = let
            val (Abbrev_RES, Abbrev_SPAN, strm') = (wrap Abbrev_NT)(strm)
            val FULL_SPAN = (#1(Abbrev_SPAN), #2(Abbrev_SPAN))
            in
              ((Abbrev_RES), FULL_SPAN, strm')
            end
      fun Constr_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.LP, _, strm') => true
              | _ => false
            (* end case *))
      val (Abbrev_RES, Abbrev_SPAN, strm') = EBNF.optional(Constr_PROD_1_SUBRULE_2_PRED, (wrap Constr_PROD_1_SUBRULE_2_NT), strm')
      val FULL_SPAN = (#1(ID_SPAN), #2(Abbrev_SPAN))
      in
        (UserCode.Constr_PROD_1_ACT (ID_RES, SR_RES, Abbrev_RES, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Abbrev_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun ConstrList_NT (strm) = let
      val (Constr_RES, Constr_SPAN, strm') = (wrap Constr_NT)(strm)
      fun ConstrList_PROD_1_SUBRULE_1_NT (strm) = let
            val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
            val (Constr_RES, Constr_SPAN, strm') = (wrap Constr_NT)(strm')
            val FULL_SPAN = (#1(BAR_SPAN), #2(Constr_SPAN))
            in
              ((Constr_RES), FULL_SPAN, strm')
            end
      fun ConstrList_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.BAR, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(ConstrList_PROD_1_SUBRULE_1_PRED, (wrap ConstrList_PROD_1_SUBRULE_1_NT), strm')
      val FULL_SPAN = (#1(Constr_SPAN), #2(SR_SPAN))
      in
        (UserCode.ConstrList_PROD_1_ACT (SR_RES, Constr_RES, SR_SPAN : (Lex.pos * Lex.pos), Constr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun ID'_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
      in
        (UserCode.ID'_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun IDList_NT (strm) = let
      val (ID'_RES, ID'_SPAN, strm') = (wrap ID'_NT)(strm)
      fun IDList_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (ID'_RES, ID'_SPAN, strm') = (wrap ID'_NT)(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(ID'_SPAN))
            in
              ((ID'_RES), FULL_SPAN, strm')
            end
      fun IDList_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(IDList_PROD_1_SUBRULE_1_PRED, (wrap IDList_PROD_1_SUBRULE_1_NT), strm')
      val FULL_SPAN = (#1(ID'_SPAN), #2(SR_SPAN))
      in
        (UserCode.IDList_PROD_1_ACT (SR_RES, ID'_RES, SR_SPAN : (Lex.pos * Lex.pos), ID'_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Decl_NT (strm) = let
      fun Decl_PROD_1 (strm) = let
            val (KW_name_RES, KW_name_SPAN, strm') = matchKW_name(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val FULL_SPAN = (#1(KW_name_SPAN), #2(ID_SPAN))
            in
              (UserCode.Decl_PROD_1_ACT (ID_RES, KW_name_RES, ID_SPAN : (Lex.pos * Lex.pos), KW_name_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Decl_PROD_2 (strm) = let
            val (KW_start_RES, KW_start_SPAN, strm') = matchKW_start(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val FULL_SPAN = (#1(KW_start_SPAN), #2(ID_SPAN))
            in
              (UserCode.Decl_PROD_2_ACT (ID_RES, KW_start_RES, ID_SPAN : (Lex.pos * Lex.pos), KW_start_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Decl_PROD_3 (strm) = let
            val (KW_entry_RES, KW_entry_SPAN, strm') = matchKW_entry(strm)
            val (IDList_RES, IDList_SPAN, strm') = (wrap IDList_NT)(strm')
            val FULL_SPAN = (#1(KW_entry_SPAN), #2(IDList_SPAN))
            in
              (UserCode.Decl_PROD_3_ACT (KW_entry_RES, IDList_RES, KW_entry_SPAN : (Lex.pos * Lex.pos), IDList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Decl_PROD_4 (strm) = let
            val (KW_keywords_RES, KW_keywords_SPAN, strm') = matchKW_keywords(strm)
            val (SymList_RES, SymList_SPAN, strm') = (wrap SymList_NT)(strm')
            val FULL_SPAN = (#1(KW_keywords_SPAN), #2(SymList_SPAN))
            in
              (UserCode.Decl_PROD_4_ACT (KW_keywords_RES, SymList_RES, KW_keywords_SPAN : (Lex.pos * Lex.pos), SymList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Decl_PROD_5 (strm) = let
            val (KW_defs_RES, KW_defs_SPAN, strm') = matchKW_defs(strm)
            val (Code_RES, Code_SPAN, strm') = (wrap Code_NT)(strm')
            val FULL_SPAN = (#1(KW_defs_SPAN), #2(Code_SPAN))
            in
              (UserCode.Decl_PROD_5_ACT (Code_RES, KW_defs_RES, Code_SPAN : (Lex.pos * Lex.pos), KW_defs_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Decl_PROD_6 (strm) = let
            val (KW_tokens_RES, KW_tokens_SPAN, strm') = matchKW_tokens(strm)
            fun Decl_PROD_6_SUBRULE_1_NT (strm) = let
                  val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm)
                  val FULL_SPAN = (#1(COLON_SPAN), #2(COLON_SPAN))
                  in
                    ((), FULL_SPAN, strm')
                  end
            fun Decl_PROD_6_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COLON, _, strm') => true
                    | _ => false
                  (* end case *))
            val (COLON_RES, COLON_SPAN, strm') = EBNF.optional(Decl_PROD_6_SUBRULE_1_PRED, (wrap Decl_PROD_6_SUBRULE_1_NT), strm')
            val (ConstrList_RES, ConstrList_SPAN, strm') = (wrap ConstrList_NT)(strm')
            val FULL_SPAN = (#1(KW_tokens_SPAN), #2(ConstrList_SPAN))
            in
              (UserCode.Decl_PROD_6_ACT (COLON_RES, KW_tokens_RES, ConstrList_RES, COLON_SPAN : (Lex.pos * Lex.pos), KW_tokens_SPAN : (Lex.pos * Lex.pos), ConstrList_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Decl_PROD_7 (strm) = let
            val (KW_import_RES, KW_import_SPAN, strm') = matchKW_import(strm)
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm')
            fun Decl_PROD_7_SUBRULE_1_NT (strm) = let
                  val (KW_dropping_RES, KW_dropping_SPAN, strm') = matchKW_dropping(strm)
                  val (SymList_RES, SymList_SPAN, strm') = (wrap SymList_NT)(strm')
                  val FULL_SPAN = (#1(KW_dropping_SPAN), #2(SymList_SPAN))
                  in
                    ((SymList_RES), FULL_SPAN, strm')
                  end
            fun Decl_PROD_7_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_dropping, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(Decl_PROD_7_SUBRULE_1_PRED, (wrap Decl_PROD_7_SUBRULE_1_NT), strm')
            val FULL_SPAN = (#1(KW_import_SPAN), #2(SR_SPAN))
            in
              (UserCode.Decl_PROD_7_ACT (SR_RES, STRING_RES, KW_import_RES, SR_SPAN : (Lex.pos * Lex.pos), STRING_SPAN : (Lex.pos * Lex.pos), KW_import_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Decl_PROD_8 (strm) = let
            val (KW_refcell_RES, KW_refcell_SPAN, strm') = matchKW_refcell(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm')
            val (Ty_RES, Ty_SPAN, strm') = (wrap Ty_NT)(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (Code_RES, Code_SPAN, strm') = (wrap Code_NT)(strm')
            val FULL_SPAN = (#1(KW_refcell_SPAN), #2(Code_SPAN))
            in
              (UserCode.Decl_PROD_8_ACT (EQ_RES, ID_RES, Ty_RES, Code_RES, COLON_RES, KW_refcell_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), Ty_SPAN : (Lex.pos * Lex.pos), Code_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), KW_refcell_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Decl_PROD_9 (strm) = let
            val (KW_nonterms_RES, KW_nonterms_SPAN, strm') = matchKW_nonterms(strm)
            fun Decl_PROD_9_SUBRULE_1_NT (strm) = let
                  val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm)
                  val FULL_SPAN = (#1(COLON_SPAN), #2(COLON_SPAN))
                  in
                    ((), FULL_SPAN, strm')
                  end
            fun Decl_PROD_9_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COLON, _, strm') => true
                    | _ => false
                  (* end case *))
            val (COLON_RES, COLON_SPAN, strm') = EBNF.optional(Decl_PROD_9_SUBRULE_1_PRED, (wrap Decl_PROD_9_SUBRULE_1_NT), strm')
            val (TyAnn_RES, TyAnn_SPAN, strm') = (wrap TyAnn_NT)(strm')
            fun Decl_PROD_9_SUBRULE_2_NT (strm) = let
                  val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
                  val (TyAnn_RES, TyAnn_SPAN, strm') = (wrap TyAnn_NT)(strm')
                  val FULL_SPAN = (#1(BAR_SPAN), #2(TyAnn_SPAN))
                  in
                    ((TyAnn_RES), FULL_SPAN, strm')
                  end
            fun Decl_PROD_9_SUBRULE_2_PRED (strm) = (case (lex(strm))
                   of (Tok.BAR, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Decl_PROD_9_SUBRULE_2_PRED, (wrap Decl_PROD_9_SUBRULE_2_NT), strm')
            val FULL_SPAN = (#1(KW_nonterms_SPAN), #2(SR_SPAN))
            in
              (UserCode.Decl_PROD_9_ACT (SR_RES, COLON_RES, TyAnn_RES, KW_nonterms_RES, SR_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), TyAnn_SPAN : (Lex.pos * Lex.pos), KW_nonterms_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Decl_PROD_10 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            fun Decl_PROD_10_SUBRULE_1_NT (strm) = let
                  val (Formals_RES, Formals_SPAN, strm') = (wrap Formals_NT)(strm)
                  val FULL_SPAN = (#1(Formals_SPAN), #2(Formals_SPAN))
                  in
                    ((Formals_RES), FULL_SPAN, strm')
                  end
            fun Decl_PROD_10_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.LP, _, strm') => true
                    | _ => false
                  (* end case *))
            val (Formals_RES, Formals_SPAN, strm') = EBNF.optional(Decl_PROD_10_SUBRULE_1_PRED, (wrap Decl_PROD_10_SUBRULE_1_NT), strm')
            val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm')
            val (AltList_RES, AltList_SPAN, strm') = (wrap AltList_NT)(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(AltList_SPAN))
            in
              (UserCode.Decl_PROD_10_ACT (ID_RES, COLON_RES, AltList_RES, Formals_RES, ID_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), AltList_SPAN : (Lex.pos * Lex.pos), Formals_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => Decl_PROD_10(strm)
          | (Tok.KW_refcell, _, strm') => Decl_PROD_8(strm)
          | (Tok.KW_tokens, _, strm') => Decl_PROD_6(strm)
          | (Tok.KW_keywords, _, strm') => Decl_PROD_4(strm)
          | (Tok.KW_start, _, strm') => Decl_PROD_2(strm)
          | (Tok.KW_name, _, strm') => Decl_PROD_1(strm)
          | (Tok.KW_entry, _, strm') => Decl_PROD_3(strm)
          | (Tok.KW_defs, _, strm') => Decl_PROD_5(strm)
          | (Tok.KW_import, _, strm') => Decl_PROD_7(strm)
          | (Tok.KW_nonterms, _, strm') => Decl_PROD_9(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun File_NT (strm) = let
      fun File_PROD_1_SUBRULE_1_NT (strm) = let
            val (Decl_RES, Decl_SPAN, strm') = (wrap Decl_NT)(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(Decl_SPAN), #2(SEMI_SPAN))
            in
              ((Decl_RES), FULL_SPAN, strm')
            end
      fun File_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_tokens, _, strm') => true
              | (Tok.KW_defs, _, strm') => true
              | (Tok.KW_keywords, _, strm') => true
              | (Tok.KW_import, _, strm') => true
              | (Tok.KW_name, _, strm') => true
              | (Tok.KW_start, _, strm') => true
              | (Tok.KW_entry, _, strm') => true
              | (Tok.KW_refcell, _, strm') => true
              | (Tok.KW_nonterms, _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(File_PROD_1_SUBRULE_1_PRED, (wrap File_PROD_1_SUBRULE_1_NT), strm)
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        (UserCode.File_PROD_1_ACT (SR_RES, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
in
  (reqEOF(File_NT))
end
val File_NT =  fn s => unwrap (Err.launch eh (File_NT ) (R.wrap (s, lexFn)))

in (File_NT) end
  in
fun parse lexFn  s = let val (File_NT) = mk lexFn in File_NT s end

  end

end
