structure SpecLex  = struct

    datatype yystart_state = 
COM | CODE | STRING | CONSTR | INITIAL
    structure UserDeclarations = 
      struct

 

structure Tok = SpecTokens

val comLvl : int ref = ref 0		(* nesting depth of comments *)
val comStart : int ref = ref 0		(* start line of current comment *)

type lex_result = Tok.token

fun err (lineNo, colNo, msg) = Err.errMsg [
      "Lexical error [", Int.toString lineNo, ".", Int.toString colNo, "]: ", msg
      ]

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

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
#[
]

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
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
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
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    fun continue() = 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CONSTR; Tok.OF)
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CONSTR; Tok.KW_tokens)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.KW_defs)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_keywords)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_nonterms)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_import)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_name)
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_start)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_entry)
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_try)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.KW_where)
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_dropping)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CONSTR; Tok.KW_refcell)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BAR)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.AT)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.DOLLAR)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.PLUS)
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.STAR)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.QUERY)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COLON)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SEMI)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LP)
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RP)
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LSB)
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RSB)
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SLASH)
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.EQ)
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.ARROW)
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.DARROW)
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN INITIAL);
		    Tok.STRING (getText())
      end
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue()
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN CONSTR);
	    continue()
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN CODE);
	    continue()
      end
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
       comLvl := !comLvl+1; continue())
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
       comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue())
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;  continue())
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         if !pcount = 0 then () else addText yytext;
		    inc pcount; continue()
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL;
		       Tok.CODE (getText()))
		    else (addText yytext; continue())
      end
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    continue()
      end
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; Tok.BOGUS
      end
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; err (!yylineno, !yycolno, "unclosed string");
 	            Tok.BOGUS
      end
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;  continue())
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.OF)
fun yyAction51 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction52 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.TYVAR yytext
      end
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.IDDOT yytext
      end
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.INT yytext
      end
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BAR)
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.STAR)
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COLON)
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.SEMI)
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LP)
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RP)
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LCB)
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RCB)
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.ARROW)
fun yyAction64 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN CONSTR);
		    Tok.STRING (getText())
      end
fun yyAction65 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.EQ)
fun yyAction66 (strm, lastMatch : yymatch) = let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         err (!yylineno, !yycolno,
		 concat["illegal character '", 
			String.toCString yytext, "'"]);
	    continue()
      end
fun yyQ76 (strm, lastMatch : yymatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx28
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ77(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx28
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ77(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyAction0(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ77(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ77(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ77(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ77(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ77(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx27
                      then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx66
                  then yyQ78(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ73 (strm, lastMatch : yymatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx28
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ77(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = yyAction16(strm, yyNO_MATCH)
fun yyQ70 (strm, lastMatch : yymatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ79 (strm, lastMatch : yymatch) = yyAction31(strm, yyNO_MATCH)
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ79(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ67 (strm, lastMatch : yymatch) = yyAction21(strm, yyNO_MATCH)
fun yyQ66 (strm, lastMatch : yymatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ80 (strm, lastMatch : yymatch) = yyAction30(strm, yyNO_MATCH)
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ80(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = yyAction23(strm, yyNO_MATCH)
fun yyQ63 (strm, lastMatch : yymatch) = yyAction18(strm, yyNO_MATCH)
fun yyQ62 (strm, lastMatch : yymatch) = yyAction19(strm, yyNO_MATCH)
fun yyQ61 (strm, lastMatch : yymatch) = yyAction25(strm, yyNO_MATCH)
fun yyQ81 (strm, lastMatch : yymatch) = yyAction33(strm, yyNO_MATCH)
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ81(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ94(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ93(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ92(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx68
              then yyQ91(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = yyAction11(strm, yyNO_MATCH)
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx79
              then yyQ97(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ101(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ100(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ99(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6B
              then yyQ98(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx70
              then yystuck(lastMatch)
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ95(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx72
              then yyQ96(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ104(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ103(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ102(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ111(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ110(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ109(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx63
              then yyQ108(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ107(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ106(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ119(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyQ118(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ117(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ116(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ115(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ114(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ121(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyQ120(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx62
              then yystuck(lastMatch)
            else if inp < 0wx62
              then if inp = 0wx61
                  then yyQ112(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx6F
              then yyQ113(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ128 (strm, lastMatch : yymatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ127 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ128(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ126 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx64
              then yyQ127(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ125 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ126(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ124 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ125(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx77
              then yyQ124(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ122 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx79
              then yyQ123(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ122(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ133 (strm, lastMatch : yymatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ132 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ133(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ131 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ132(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ130 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ131(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ129 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx70
              then yyQ130(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyQ129(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ137 (strm, lastMatch : yymatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ136 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx79
              then yyQ137(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ135 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ136(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ134 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ135(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ134(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ145 (strm, lastMatch : yymatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ144 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx67
              then yyQ145(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ143 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ144(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ142 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ143(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ141 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx70
              then yyQ142(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ140 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx70
              then yyQ141(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ139 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ140(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ147 (strm, lastMatch : yymatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ146 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ147(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ138 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ146(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yystuck(lastMatch)
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ138(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx72
              then yyQ139(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ86(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx69
                  then yyQ84(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < 0wx69
                  then if inp = 0wx65
                      then yyQ83(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                    else if inp < 0wx65
                      then if inp = 0wx64
                          then yyQ82(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                          else yyAction66(strm, yyNO_MATCH)
                      else yyAction66(strm, yyNO_MATCH)
                else if inp = 0wx6B
                  then yyQ85(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx72
                  then yyQ87(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp = 0wx73
                  then yyQ88(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = 0wx77
              then yyQ90(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ57 (strm, lastMatch : yymatch) = yyAction32(strm, yyNO_MATCH)
fun yyQ149 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction1(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ149(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
and yyQ148 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction1(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ149(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction1(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ149(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction1(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ149(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ148(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = yyAction66(strm, yyNO_MATCH)
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ27(strm', lastMatch)
            else if inp < 0wx30
              then if inp = 0wx25
                  then yyQ59(strm', lastMatch)
                else if inp < 0wx25
                  then if inp = 0wx20
                      then yyQ55(strm', lastMatch)
                    else if inp < 0wx20
                      then if inp = 0wxD
                          then yyQ56(strm', lastMatch)
                        else if inp < 0wxD
                          then if inp <= 0wx8
                              then yyQ27(strm', lastMatch)
                              else yyQ55(strm', lastMatch)
                          else yyQ27(strm', lastMatch)
                    else if inp = 0wx23
                      then yyQ27(strm', lastMatch)
                    else if inp < 0wx23
                      then if inp = 0wx21
                          then yyQ27(strm', lastMatch)
                          else yyQ57(strm', lastMatch)
                      else yyQ58(strm', lastMatch)
                else if inp = 0wx2B
                  then yyQ63(strm', lastMatch)
                else if inp < 0wx2B
                  then if inp = 0wx29
                      then yyQ61(strm', lastMatch)
                    else if inp < 0wx29
                      then if inp = 0wx28
                          then yyQ60(strm', lastMatch)
                          else yyQ27(strm', lastMatch)
                      else yyQ62(strm', lastMatch)
                else if inp = 0wx2E
                  then yyQ27(strm', lastMatch)
                else if inp < 0wx2E
                  then if inp = 0wx2C
                      then yyQ64(strm', lastMatch)
                      else yyQ65(strm', lastMatch)
                  else yyQ66(strm', lastMatch)
            else if inp = 0wx5B
              then yyQ73(strm', lastMatch)
            else if inp < 0wx5B
              then if inp = 0wx3D
                  then yyQ69(strm', lastMatch)
                else if inp < 0wx3D
                  then if inp = 0wx3B
                      then yyQ68(strm', lastMatch)
                    else if inp < 0wx3B
                      then if inp = 0wx3A
                          then yyQ67(strm', lastMatch)
                          else yyQ27(strm', lastMatch)
                      else yyQ27(strm', lastMatch)
                else if inp = 0wx40
                  then yyQ71(strm', lastMatch)
                else if inp < 0wx40
                  then if inp = 0wx3E
                      then yyQ27(strm', lastMatch)
                      else yyQ70(strm', lastMatch)
                  else yyQ72(strm', lastMatch)
            else if inp = 0wx6F
              then yyQ75(strm', lastMatch)
            else if inp < 0wx6F
              then if inp = 0wx5E
                  then yyQ27(strm', lastMatch)
                else if inp < 0wx5E
                  then if inp = 0wx5C
                      then yyQ27(strm', lastMatch)
                      else yyQ74(strm', lastMatch)
                else if inp <= 0wx60
                  then yyQ27(strm', lastMatch)
                  else yyQ72(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ76(strm', lastMatch)
            else if inp < 0wx7C
              then if inp = 0wx7B
                  then yyQ27(strm', lastMatch)
                  else yyQ72(strm', lastMatch)
              else yyQ27(strm', lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = yyAction62(strm, yyNO_MATCH)
fun yyQ43 (strm, lastMatch : yymatch) = yyAction55(strm, yyNO_MATCH)
fun yyQ42 (strm, lastMatch : yymatch) = yyAction61(strm, yyNO_MATCH)
fun yyQ46 (strm, lastMatch : yymatch) = yyAction53(strm, yyNO_MATCH)
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction51(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp = 0wx2E
                  then yyQ46(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx2F
                  then yyAction51(strm, yyNO_MATCH)
                  else yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp = 0wx5F
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx41
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction51(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction50(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp = 0wx2E
                  then yyQ46(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ45(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                      else yyAction50(strm, yyNO_MATCH)
                else if inp = 0wx2F
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ45(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp = 0wx5F
              then yyQ45(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx41
                  then yyQ45(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction50(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ45(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ45(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction50(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ45(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2E
                  then yyQ46(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx66
                  then yyQ47(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2E
                  then yyQ46(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ50(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2E
                  then yyQ46(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ50(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2E
                  then yyQ46(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx67
                  then yyQ49(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2E
                  then yyQ46(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ48(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction51(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp = 0wx2E
                  then yyQ46(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp = 0wx27
                      then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                      else yyAction51(strm, yyNO_MATCH)
                else if inp = 0wx2F
                  then yyAction51(strm, yyNO_MATCH)
                  else yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp = 0wx5F
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx41
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction51(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
                  else yyAction51(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction51(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ45(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = yyAction65(strm, yyNO_MATCH)
fun yyQ37 (strm, lastMatch : yymatch) = yyAction58(strm, yyNO_MATCH)
fun yyQ36 (strm, lastMatch : yymatch) = yyAction57(strm, yyNO_MATCH)
fun yyQ51 (strm, lastMatch : yymatch) = yyAction63(strm, yyNO_MATCH)
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ51(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = yyAction56(strm, yyNO_MATCH)
fun yyQ33 (strm, lastMatch : yymatch) = yyAction60(strm, yyNO_MATCH)
fun yyQ52 (strm, lastMatch : yymatch) = yyAction34(strm, yyNO_MATCH)
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ52(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
              else yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
fun yyQ30 (strm, lastMatch : yymatch) = yyAction64(strm, yyNO_MATCH)
fun yyQ54 (strm, lastMatch : yymatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ54(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyQ36(strm', lastMatch)
            else if inp < 0wx3A
              then if inp = 0wx23
                  then yyQ27(strm', lastMatch)
                else if inp < 0wx23
                  then if inp = 0wxE
                      then yyQ27(strm', lastMatch)
                    else if inp < 0wxE
                      then if inp = 0wx9
                          then yyQ28(strm', lastMatch)
                        else if inp < 0wx9
                          then yyQ27(strm', lastMatch)
                        else if inp = 0wxD
                          then yyQ29(strm', lastMatch)
                          else yyQ28(strm', lastMatch)
                    else if inp = 0wx21
                      then yyQ27(strm', lastMatch)
                    else if inp < 0wx21
                      then if inp = 0wx20
                          then yyQ28(strm', lastMatch)
                          else yyQ27(strm', lastMatch)
                      else yyQ30(strm', lastMatch)
                else if inp = 0wx2A
                  then yyQ34(strm', lastMatch)
                else if inp < 0wx2A
                  then if inp = 0wx28
                      then yyQ32(strm', lastMatch)
                    else if inp < 0wx28
                      then if inp = 0wx27
                          then yyQ31(strm', lastMatch)
                          else yyQ27(strm', lastMatch)
                      else yyQ33(strm', lastMatch)
                else if inp = 0wx2D
                  then yyQ35(strm', lastMatch)
                  else yyQ27(strm', lastMatch)
            else if inp = 0wx64
              then yyQ40(strm', lastMatch)
            else if inp < 0wx64
              then if inp = 0wx3E
                  then yyQ27(strm', lastMatch)
                else if inp < 0wx3E
                  then if inp = 0wx3C
                      then yyQ27(strm', lastMatch)
                    else if inp = 0wx3B
                      then yyQ37(strm', lastMatch)
                      else yyQ38(strm', lastMatch)
                else if inp = 0wx5B
                  then yyQ27(strm', lastMatch)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyQ27(strm', lastMatch)
                      else yyQ39(strm', lastMatch)
                else if inp <= 0wx60
                  then yyQ27(strm', lastMatch)
                  else yyQ39(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ42(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx6F
                  then yyQ41(strm', lastMatch)
                  else yyQ39(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ44(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ43(strm', lastMatch)
              else yyQ27(strm', lastMatch)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = yyAction46(strm, yyNO_MATCH)
fun yyQ23 (strm, lastMatch : yymatch) = yyAction48(strm, yyNO_MATCH)
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyAction45(strm, yyNO_MATCH)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ23(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = 0wx5C
              then yyQ24(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ25 (strm, lastMatch : yymatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ25(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction47(strm, yyNO_MATCH)
              else yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction47(strm, yyNO_MATCH)
              else yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ18(strm', lastMatch)
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ18(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ19(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ20(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx23
              then yyQ18(strm', lastMatch)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ21(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ22(strm', lastMatch)
              else yyQ18(strm', lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = yyAction40(strm, yyNO_MATCH)
fun yyQ16 (strm, lastMatch : yymatch) = yyAction35(strm, yyNO_MATCH)
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ16(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyQ14(strm', lastMatch)
            else if inp < 0wx28
              then if inp = 0wx22
                  then yyQ13(strm', lastMatch)
                  else yyQ12(strm', lastMatch)
            else if inp = 0wx29
              then yyQ15(strm', lastMatch)
              else yyQ12(strm', lastMatch)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx29
              then yyQ9(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ10(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = yyAction38(strm, yyNO_MATCH)
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ11(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = yyAction38(strm, yyNO_MATCH)
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyQ7(strm', lastMatch)
            else if inp < 0wx28
              then if inp = 0wxD
                  then yyQ6(strm', lastMatch)
                  else yyQ5(strm', lastMatch)
            else if inp = 0wx2A
              then yyQ8(strm', lastMatch)
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
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
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
