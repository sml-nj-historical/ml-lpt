structure SpecLex  = struct

    datatype yystart_state = 
COM | CODE | STRING | CONSTR | INITIAL
    structure UserDeclarations = 
      struct

 

structure Tok = SpecTokens

val comLvl : int ref = ref 0		(* nesting depth of comments *)
val comStart : int ref = ref 0		(* start line of current comment *)

type lex_result = Tok.token

fun err _ = ()

fun eof () = (
      if (!comLvl > 0)
        then ()
(* err(~1, "unclosed comment starting at line " ^ Int.toString(!comStart)) *)
        else ();
      Tok.EOF)

val text : string list ref = ref []
fun addText s = (text := s::(!text))
fun clrText () = (text := [])
fun getText () = concat (rev (!text))

val pcount = ref 0
fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)



      end

    local
    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    fun innerLex (yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case UTF8.getu ULexBuffer.getc strm
                of (SOME (0w10, s')) => 
		     (StreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = StreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = StreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun continue() = 
let
fun yyAction0 (strm, lastMatch) = (yystrm := strm;  YYBEGIN CONSTR; Tok.OF)
fun yyAction1 (strm, lastMatch) = (yystrm := strm;  skip())
fun yyAction2 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction3 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CONSTR; Tok.KW_tokens)
fun yyAction4 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.KW_defs)
fun yyAction5 (strm, lastMatch) = (yystrm := strm;  Tok.KW_keywords)
fun yyAction6 (strm, lastMatch) = (yystrm := strm;  Tok.KW_nonterms)
fun yyAction7 (strm, lastMatch) = (yystrm := strm;  Tok.KW_import)
fun yyAction8 (strm, lastMatch) = (yystrm := strm;  Tok.KW_name)
fun yyAction9 (strm, lastMatch) = (yystrm := strm;  Tok.KW_start)
fun yyAction10 (strm, lastMatch) = (yystrm := strm;  Tok.KW_entry)
fun yyAction11 (strm, lastMatch) = (yystrm := strm;  Tok.KW_try)
fun yyAction12 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.KW_where)
fun yyAction13 (strm, lastMatch) = (yystrm := strm;  Tok.KW_dropping)
fun yyAction14 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CONSTR; Tok.KW_refcell)
fun yyAction15 (strm, lastMatch) = (yystrm := strm;  Tok.BAR)
fun yyAction16 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.AT)
fun yyAction17 (strm, lastMatch) = (yystrm := strm;  Tok.DOLLAR)
fun yyAction18 (strm, lastMatch) = (yystrm := strm;  Tok.PLUS)
fun yyAction19 (strm, lastMatch) = (yystrm := strm;  Tok.STAR)
fun yyAction20 (strm, lastMatch) = (yystrm := strm;  Tok.QUERY)
fun yyAction21 (strm, lastMatch) = (yystrm := strm;  Tok.COLON)
fun yyAction22 (strm, lastMatch) = (yystrm := strm;  Tok.SEMI)
fun yyAction23 (strm, lastMatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction24 (strm, lastMatch) = (yystrm := strm;  Tok.LP)
fun yyAction25 (strm, lastMatch) = (yystrm := strm;  Tok.RP)
fun yyAction26 (strm, lastMatch) = (yystrm := strm;  Tok.LSB)
fun yyAction27 (strm, lastMatch) = (yystrm := strm;  Tok.RSB)
fun yyAction28 (strm, lastMatch) = (yystrm := strm;  Tok.SLASH)
fun yyAction29 (strm, lastMatch) = (yystrm := strm;  Tok.EQ)
fun yyAction30 (strm, lastMatch) = (yystrm := strm;  Tok.ARROW)
fun yyAction31 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.DARROW)
fun yyAction32 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN INITIAL);
		    Tok.STRING (getText())
      end
fun yyAction33 (strm, lastMatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue()
      end
fun yyAction34 (strm, lastMatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN CONSTR);
	    continue()
      end
fun yyAction35 (strm, lastMatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN CODE);
	    continue()
      end
fun yyAction36 (strm, lastMatch) = (yystrm := strm;
       comLvl := !comLvl+1; continue())
fun yyAction37 (strm, lastMatch) = (yystrm := strm;
       comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue())
fun yyAction38 (strm, lastMatch) = (yystrm := strm;  continue())
fun yyAction39 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         if !pcount = 0 then () else addText yytext;
		    inc pcount; continue()
      end
fun yyAction40 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL;
		       Tok.CODE (getText()))
		    else (addText yytext; continue())
      end
fun yyAction41 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    continue()
      end
fun yyAction42 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction43 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; Tok.BOGUS
      end
fun yyAction44 (strm, lastMatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; err (!yylineno, "unclosed string");
 	            Tok.BOGUS
      end
fun yyAction45 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction46 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction47 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction48 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction49 (strm, lastMatch) = (yystrm := strm;  continue())
fun yyAction50 (strm, lastMatch) = (yystrm := strm;  Tok.OF)
fun yyAction51 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction52 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.TYVAR yytext
      end
fun yyAction53 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.IDDOT yytext
      end
fun yyAction54 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.INT yytext
      end
fun yyAction55 (strm, lastMatch) = (yystrm := strm;  Tok.BAR)
fun yyAction56 (strm, lastMatch) = (yystrm := strm;  Tok.STAR)
fun yyAction57 (strm, lastMatch) = (yystrm := strm;  Tok.COLON)
fun yyAction58 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.SEMI)
fun yyAction59 (strm, lastMatch) = (yystrm := strm;  Tok.LP)
fun yyAction60 (strm, lastMatch) = (yystrm := strm;  Tok.RP)
fun yyAction61 (strm, lastMatch) = (yystrm := strm;  Tok.LCB)
fun yyAction62 (strm, lastMatch) = (yystrm := strm;  Tok.RCB)
fun yyAction63 (strm, lastMatch) = (yystrm := strm;  Tok.ARROW)
fun yyAction64 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN CONSTR);
		    Tok.STRING (getText())
      end
fun yyAction65 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.EQ)
fun yyAction66 (strm, lastMatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         err (!yylineno, 
		 concat["illegal character '", 
			String.toCString yytext, "'"]);
	    continue()
      end
fun yyQ55 (strm, lastMatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ56 (strm, lastMatch) = yyAction16(strm, yyNO_MATCH)
fun yyQ57 (strm, lastMatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ58 (strm, lastMatch) = yyAction18(strm, yyNO_MATCH)
fun yyQ59 (strm, lastMatch) = yyAction19(strm, yyNO_MATCH)
fun yyQ60 (strm, lastMatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ61 (strm, lastMatch) = yyAction21(strm, yyNO_MATCH)
fun yyQ62 (strm, lastMatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ63 (strm, lastMatch) = yyAction23(strm, yyNO_MATCH)
fun yyQ149 (strm, lastMatch) = yyAction33(strm, yyNO_MATCH)
fun yyQ64 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ149(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch) = yyAction25(strm, yyNO_MATCH)
fun yyQ66 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ67 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ68 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ148 (strm, lastMatch) = yyAction31(strm, yyNO_MATCH)
fun yyQ69 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ148(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch) = yyAction32(strm, yyNO_MATCH)
fun yyQ37 (strm, lastMatch) = yyAction66(strm, yyNO_MATCH)
fun yyQ147 (strm, lastMatch) = yyAction30(strm, yyNO_MATCH)
fun yyQ71 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ147(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ146 (strm, lastMatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ145 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ146(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ144 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ145(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ143 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ144(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ142 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx63
              then yyQ143(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ141 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ142(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ81 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ141(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ140 (strm, lastMatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ139 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ140(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ138 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ139(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ137 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ138(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ82 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx68
              then yyQ137(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ136 (strm, lastMatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ135 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx79
              then yyQ136(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ134 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ135(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ133 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ134(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ83 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ133(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ132 (strm, lastMatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ131 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ132(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ130 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ131(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ129 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ130(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ84 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ129(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ128 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ127 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ128(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ126 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ127(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ125 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ126(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ124 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx70
              then yyQ125(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ85 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyQ124(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ123 (strm, lastMatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ122 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ123(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ114 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyQ122(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ121 (strm, lastMatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ120 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ121(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyQ120(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ118 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ119(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ117 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ118(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ116 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ117(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ115 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ116(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ86 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx62
              then yystuck(lastMatch)
            else if inp < 0wx62
              then if inp = 0wx61
                  then yyQ114(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx6F
              then yyQ115(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ113 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ112 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ113(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx64
              then yyQ112(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ110 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ111(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ109 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ110(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ108 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx77
              then yyQ109(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ107 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx79
              then yyQ108(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ87 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ107(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ106 (strm, lastMatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ105 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx67
              then yyQ106(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ104 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ103 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ104(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ102 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx70
              then yyQ103(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ101 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx70
              then yyQ102(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ97 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ101(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ100 (strm, lastMatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ99 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ100(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ98 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ99(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ88 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yystuck(lastMatch)
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ98(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx72
              then yyQ97(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ96 (strm, lastMatch) = yyAction11(strm, yyNO_MATCH)
fun yyQ90 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx79
              then yyQ96(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ95 (strm, lastMatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ94 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ95(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ94(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ92 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ93(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ91 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6B
              then yyQ92(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ89 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx70
              then yystuck(lastMatch)
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ91(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx72
              then yyQ90(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ72 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ86(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx69
                  then yyQ85(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < 0wx69
                  then if inp = 0wx65
                      then yyQ83(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                    else if inp < 0wx65
                      then if inp = 0wx64
                          then yyQ88(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                          else yyAction66(strm, yyNO_MATCH)
                      else yyAction66(strm, yyNO_MATCH)
                else if inp = 0wx6B
                  then yyQ87(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx72
                  then yyQ81(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp = 0wx73
                  then yyQ84(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = 0wx77
              then yyQ82(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx28
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx28
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction1(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ79(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
and yyQ80 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction1(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ79(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction1(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ79(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction1(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ79(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ80(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ78(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx28
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ78(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyAction0(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ78(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ78(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ78(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ78(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ78(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx27
                      then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx66
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ37(strm', lastMatch)
            else if inp < 0wx30
              then if inp = 0wx25
                  then yyQ72(strm', lastMatch)
                else if inp < 0wx25
                  then if inp = 0wx20
                      then yyQ75(strm', lastMatch)
                    else if inp < 0wx20
                      then if inp = 0wxD
                          then yyQ74(strm', lastMatch)
                        else if inp < 0wxD
                          then if inp <= 0wx8
                              then yyQ37(strm', lastMatch)
                              else yyQ75(strm', lastMatch)
                          else yyQ37(strm', lastMatch)
                    else if inp = 0wx23
                      then yyQ37(strm', lastMatch)
                    else if inp < 0wx23
                      then if inp = 0wx21
                          then yyQ37(strm', lastMatch)
                          else yyQ70(strm', lastMatch)
                      else yyQ57(strm', lastMatch)
                else if inp = 0wx2B
                  then yyQ58(strm', lastMatch)
                else if inp < 0wx2B
                  then if inp = 0wx29
                      then yyQ65(strm', lastMatch)
                    else if inp < 0wx29
                      then if inp = 0wx28
                          then yyQ64(strm', lastMatch)
                          else yyQ37(strm', lastMatch)
                      else yyQ59(strm', lastMatch)
                else if inp = 0wx2E
                  then yyQ37(strm', lastMatch)
                else if inp < 0wx2E
                  then if inp = 0wx2C
                      then yyQ63(strm', lastMatch)
                      else yyQ71(strm', lastMatch)
                  else yyQ68(strm', lastMatch)
            else if inp = 0wx5B
              then yyQ66(strm', lastMatch)
            else if inp < 0wx5B
              then if inp = 0wx3D
                  then yyQ69(strm', lastMatch)
                else if inp < 0wx3D
                  then if inp = 0wx3B
                      then yyQ62(strm', lastMatch)
                    else if inp < 0wx3B
                      then if inp = 0wx3A
                          then yyQ61(strm', lastMatch)
                          else yyQ37(strm', lastMatch)
                      else yyQ37(strm', lastMatch)
                else if inp = 0wx40
                  then yyQ56(strm', lastMatch)
                else if inp < 0wx40
                  then if inp = 0wx3E
                      then yyQ37(strm', lastMatch)
                      else yyQ60(strm', lastMatch)
                  else yyQ73(strm', lastMatch)
            else if inp = 0wx6F
              then yyQ76(strm', lastMatch)
            else if inp < 0wx6F
              then if inp = 0wx5E
                  then yyQ37(strm', lastMatch)
                else if inp < 0wx5E
                  then if inp = 0wx5C
                      then yyQ37(strm', lastMatch)
                      else yyQ67(strm', lastMatch)
                else if inp <= 0wx60
                  then yyQ37(strm', lastMatch)
                  else yyQ73(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ55(strm', lastMatch)
            else if inp < 0wx7C
              then if inp = 0wx7B
                  then yyQ37(strm', lastMatch)
                  else yyQ73(strm', lastMatch)
              else yyQ37(strm', lastMatch)
      (* end case *))
fun yyQ27 (strm, lastMatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ28 (strm, lastMatch) = yyAction55(strm, yyNO_MATCH)
fun yyQ29 (strm, lastMatch) = yyAction56(strm, yyNO_MATCH)
fun yyQ30 (strm, lastMatch) = yyAction57(strm, yyNO_MATCH)
fun yyQ31 (strm, lastMatch) = yyAction58(strm, yyNO_MATCH)
fun yyQ32 (strm, lastMatch) = yyAction60(strm, yyNO_MATCH)
fun yyQ33 (strm, lastMatch) = yyAction61(strm, yyNO_MATCH)
fun yyQ34 (strm, lastMatch) = yyAction62(strm, yyNO_MATCH)
fun yyQ35 (strm, lastMatch) = yyAction64(strm, yyNO_MATCH)
fun yyQ36 (strm, lastMatch) = yyAction65(strm, yyNO_MATCH)
fun yyQ54 (strm, lastMatch) = yyAction63(strm, yyNO_MATCH)
fun yyQ38 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ54(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx28
                  then yyAction52(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                      else yyAction52(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction52(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                  else yyAction52(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction52(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction52(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                  else yyAction52(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx28
                  then yyAction52(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                      else yyAction52(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction52(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                  else yyAction52(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction52(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction52(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                  else yyAction52(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ53(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch) = yyAction53(strm, yyNO_MATCH)
fun yyQ49 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction51(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp = 0wx2E
                  then yyQ48(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx2F
                  then yyAction51(strm, yyNO_MATCH)
                  else yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp = 0wx5F
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx41
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction51(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction51(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp = 0wx2E
                  then yyQ48(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx2F
                  then yyAction51(strm, yyNO_MATCH)
                  else yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp = 0wx5F
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx41
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction51(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2E
                  then yyQ48(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ52(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2E
                  then yyQ48(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ52(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2E
                  then yyQ48(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx67
                  then yyQ51(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2E
                  then yyQ48(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ50(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction50(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp = 0wx2E
                  then yyQ48(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ49(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                else if inp = 0wx2F
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ49(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp = 0wx5F
              then yyQ49(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx41
                  then yyQ49(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ49(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ49(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction50(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ49(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2E
                  then yyQ48(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx66
                  then yyQ47(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ43 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ46(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch) = yyAction34(strm, yyNO_MATCH)
fun yyQ44 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ45(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
              else yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyQ30(strm', lastMatch)
            else if inp < 0wx3A
              then if inp = 0wx23
                  then yyQ37(strm', lastMatch)
                else if inp < 0wx23
                  then if inp = 0wxE
                      then yyQ37(strm', lastMatch)
                    else if inp < 0wxE
                      then if inp = 0wx9
                          then yyQ27(strm', lastMatch)
                        else if inp < 0wx9
                          then yyQ37(strm', lastMatch)
                        else if inp = 0wxD
                          then yyQ43(strm', lastMatch)
                          else yyQ27(strm', lastMatch)
                    else if inp = 0wx21
                      then yyQ37(strm', lastMatch)
                    else if inp < 0wx21
                      then if inp = 0wx20
                          then yyQ27(strm', lastMatch)
                          else yyQ37(strm', lastMatch)
                      else yyQ35(strm', lastMatch)
                else if inp = 0wx2A
                  then yyQ29(strm', lastMatch)
                else if inp < 0wx2A
                  then if inp = 0wx28
                      then yyQ44(strm', lastMatch)
                    else if inp < 0wx28
                      then if inp = 0wx27
                          then yyQ39(strm', lastMatch)
                          else yyQ37(strm', lastMatch)
                      else yyQ32(strm', lastMatch)
                else if inp = 0wx2D
                  then yyQ38(strm', lastMatch)
                  else yyQ37(strm', lastMatch)
            else if inp = 0wx64
              then yyQ41(strm', lastMatch)
            else if inp < 0wx64
              then if inp = 0wx3E
                  then yyQ37(strm', lastMatch)
                else if inp < 0wx3E
                  then if inp = 0wx3C
                      then yyQ37(strm', lastMatch)
                    else if inp = 0wx3B
                      then yyQ31(strm', lastMatch)
                      else yyQ36(strm', lastMatch)
                else if inp = 0wx5B
                  then yyQ37(strm', lastMatch)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyQ37(strm', lastMatch)
                      else yyQ40(strm', lastMatch)
                else if inp <= 0wx60
                  then yyQ37(strm', lastMatch)
                  else yyQ40(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ33(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx6F
                  then yyQ42(strm', lastMatch)
                  else yyQ40(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ34(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ28(strm', lastMatch)
              else yyQ37(strm', lastMatch)
      (* end case *))
fun yyQ18 (strm, lastMatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ19 (strm, lastMatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ25 (strm, lastMatch) = yyAction46(strm, yyNO_MATCH)
fun yyQ26 (strm, lastMatch) = yyAction48(strm, yyNO_MATCH)
fun yyQ20 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyAction45(strm, yyNO_MATCH)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ26(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = 0wx5C
              then yyQ25(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction47(strm, yyNO_MATCH)
              else yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
      (* end case *))
fun yyQ21 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction47(strm, yyNO_MATCH)
              else yyQ24(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
      (* end case *))
fun yyQ23 (strm, lastMatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ22 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ23(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ21(strm', lastMatch)
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ21(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ19(strm', lastMatch)
                      else yyQ21(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ22(strm', lastMatch)
                  else yyQ21(strm', lastMatch)
            else if inp = 0wx23
              then yyQ21(strm', lastMatch)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ18(strm', lastMatch)
                  else yyQ21(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ20(strm', lastMatch)
              else yyQ21(strm', lastMatch)
      (* end case *))
fun yyQ12 (strm, lastMatch) = yyAction40(strm, yyNO_MATCH)
fun yyQ13 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ17 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ17(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = 0wx28
              then yyAction42(strm, yyNO_MATCH)
            else if inp < 0wx28
              then yyQ17(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= 0wx29
              then yyAction42(strm, yyNO_MATCH)
              else yyQ17(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
      (* end case *))
fun yyQ14 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ17(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = 0wx28
              then yyAction42(strm, yyNO_MATCH)
            else if inp < 0wx28
              then yyQ17(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp <= 0wx29
              then yyAction42(strm, yyNO_MATCH)
              else yyQ17(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
      (* end case *))
fun yyQ16 (strm, lastMatch) = yyAction35(strm, yyNO_MATCH)
fun yyQ15 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ16(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyQ15(strm', lastMatch)
            else if inp < 0wx28
              then if inp = 0wx22
                  then yyQ13(strm', lastMatch)
                  else yyQ14(strm', lastMatch)
            else if inp = 0wx29
              then yyQ12(strm', lastMatch)
              else yyQ14(strm', lastMatch)
      (* end case *))
fun yyQ5 (strm, lastMatch) = yyAction38(strm, yyNO_MATCH)
fun yyQ11 (strm, lastMatch) = yyAction38(strm, yyNO_MATCH)
fun yyQ6 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ11(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ7 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx29
              then yyQ10(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ8 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ9(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyQ8(strm', lastMatch)
            else if inp < 0wx28
              then if inp = 0wxD
                  then yyQ6(strm', lastMatch)
                  else yyQ5(strm', lastMatch)
            else if inp = 0wx2A
              then yyQ7(strm', lastMatch)
              else yyQ5(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COM => yyQ0(!(yystrm), yyNO_MATCH)
    | CODE => yyQ1(!(yystrm), yyNO_MATCH)
    | STRING => yyQ2(!(yystrm), yyNO_MATCH)
    | CONSTR => yyQ3(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ4(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            and skip() = (yystartPos := yygetPos(); continue())
	    in (continue(), (!yystartPos, yygetPos()), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = StreamPos.pos
    type span = StreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm (STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex (yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm (STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (ULexBuffer.mkStream input, ref NONE), 
			   INITIAL)

    fun streamifyReader readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            streamify input
          end

    fun streamifyInstream strm = streamify (fn ()=>TextIO.input strm)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end
