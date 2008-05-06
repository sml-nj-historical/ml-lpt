structure MLULexLex  = struct

    datatype yystart_state = 
COM | CODE | STRING | CHARSET | CHARCLASS | CURLY | RESTRING | INITIAL | DIRECTIVE
    structure UserDeclarations = 
      struct

 
  structure Tok = MLULexTokens

  val comLvl : int ref = ref 0		(* nesting depth of comments *)
  val comStart : int ref = ref 0	(* start line of current comment *)

  type lex_result = Tok.token

  val text : string list ref = ref []
  fun addText s = (text := s::(!text))
  fun clrText () = (text := [])
  fun getText () = concat (rev (!text))

  val pcount = ref 0
  fun inc (ri as ref i) = (ri := i+1)
  fun dec (ri as ref i) = (ri := i-1)

  fun hexDigit x = 
        if #"a" <= x andalso x <= #"f" then
	  Char.ord x - Char.ord #"a" + 10
	else if #"A" <=x andalso x <= #"F" then
	  Char.ord x - Char.ord #"A" + 10
	else Char.ord x - Char.ord #"0"

  fun hexVal (ss) : UTF8.wchar = 
        Substring.foldl 
	  (fn (dig, acc) => (Word.fromInt o hexDigit) dig + 0w16 * acc) 
	  0w0 ss

  fun mkUChar yyunicode = Tok.UCHAR (hd yyunicode)



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
fun yyAction0 (strm, lastMatch) = (yystrm := strm;  skip())
fun yyAction1 (strm, lastMatch) = (yystrm := strm;  skip())
fun yyAction2 (strm, lastMatch) = (yystrm := strm;  Tok.EOF)
fun yyAction3 (strm, lastMatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         
	   print (Int.toString (!yylineno) ^ ": unclosed string\n");
	    Tok.EOF
      end
fun yyAction4 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.KW_defs)
fun yyAction5 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.KW_arg)
fun yyAction6 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_name)
fun yyAction7 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_states)
fun yyAction8 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_let)
fun yyAction9 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CHARSET; Tok.KW_charset)
fun yyAction10 (strm, lastMatch) = (yystrm := strm;  Tok.EOFMARK)
fun yyAction11 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction12 (strm, lastMatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction13 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.SEMI)
fun yyAction14 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.EQ)
fun yyAction15 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.GT)
fun yyAction16 (strm, lastMatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm;  YYBEGIN INITIAL; REJECT()
      end
fun yyAction17 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.UTF8)
fun yyAction18 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.ASCII7)
fun yyAction19 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.ASCII8)
fun yyAction20 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.SEMI)
fun yyAction21 (strm, lastMatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm;  YYBEGIN INITIAL; REJECT()
      end
fun yyAction22 (strm, lastMatch) = (yystrm := strm;  Tok.BAR)
fun yyAction23 (strm, lastMatch) = (yystrm := strm;  Tok.DOT)
fun yyAction24 (strm, lastMatch) = (yystrm := strm;  Tok.DOLLAR)
fun yyAction25 (strm, lastMatch) = (yystrm := strm;  Tok.PLUS)
fun yyAction26 (strm, lastMatch) = (yystrm := strm;  Tok.AMP)
fun yyAction27 (strm, lastMatch) = (yystrm := strm;  Tok.STAR)
fun yyAction28 (strm, lastMatch) = (yystrm := strm;  Tok.QUERY)
fun yyAction29 (strm, lastMatch) = (yystrm := strm;  Tok.NEG)
fun yyAction30 (strm, lastMatch) = (yystrm := strm;  Tok.SEMI)
fun yyAction31 (strm, lastMatch) = (yystrm := strm;  Tok.LP)
fun yyAction32 (strm, lastMatch) = (yystrm := strm;  Tok.RP)
fun yyAction33 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CHARCLASS; Tok.LSB)
fun yyAction34 (strm, lastMatch) = (yystrm := strm;  Tok.RSB)
fun yyAction35 (strm, lastMatch) = (yystrm := strm;  YYBEGIN CURLY; Tok.LCB)
fun yyAction36 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.RCB)
fun yyAction37 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction38 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.INT (valOf (Int.fromString yytext))
      end
fun yyAction39 (strm, lastMatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction40 (strm, lastMatch) = (yystrm := strm;  YYBEGIN DIRECTIVE; Tok.LT)
fun yyAction41 (strm, lastMatch) = (yystrm := strm;  Tok.GT)
fun yyAction42 (strm, lastMatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction43 (strm, lastMatch) = (yystrm := strm;  Tok.SLASH)
fun yyAction44 (strm, lastMatch) = (yystrm := strm;  Tok.EQ)
fun yyAction45 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.DARROW)
fun yyAction46 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN RESTRING; continue())
fun yyAction47 (strm, lastMatch) = (yystrm := strm;  Tok.CARAT)
fun yyAction48 (strm, lastMatch) = (yystrm := strm;  Tok.DASH)
fun yyAction49 (strm, lastMatch) = let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         let val c = Char.fromString yytext
            in case c
                of SOME c' => Tok.CHAR c'
		 | NONE => (print (concat [
		     Int.toString (!yylineno), ".",
		     Int.toString (!yycolno), ": unknown escape sequence '", 
		     yytext, "'\n"]);
		     continue())
            end
      end
fun yyAction50 (strm, lastMatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;  Tok.UCHAR (hexVal (Substring.triml 2 yysubstr))
      end
fun yyAction51 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.CHAR (String.sub (yytext, 1))
      end
fun yyAction52 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.RSB)
fun yyAction53 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  mkUChar yyunicode
      end
fun yyAction54 (strm, lastMatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue()
      end
fun yyAction55 (strm, lastMatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    addText yytext;
	    ignore(continue() before YYBEGIN CODE);
	    continue()
      end
fun yyAction56 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; comLvl := !comLvl+1; continue()
      end
fun yyAction57 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue()
      end
fun yyAction58 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction59 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         if !pcount = 0 then () else addText yytext;
		    inc pcount; continue()
      end
fun yyAction60 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL; Tok.CODE (getText()))
		    else (addText yytext; continue())
      end
fun yyAction61 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    addText "\""; continue()
      end
fun yyAction62 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction63 (strm, lastMatch) = (yystrm := strm;  Tok.BOGUS)
fun yyAction64 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; print ("unclosed string\n");
 	            Tok.BOGUS
      end
fun yyAction65 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction66 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction67 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction68 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction69 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN INITIAL; continue())
fun yyAction70 (strm, lastMatch) = (yystrm := strm;
       print ("unclosed string\n"); continue())
fun yyAction71 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  mkUChar yyunicode
      end
fun yyAction72 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  mkUChar yyunicode
      end
fun yyAction73 (strm, lastMatch) = let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         print (concat[Int.toString (!yylineno), ".",
				  Int.toString (!yycolno),
				  ": illegal character '", 
				  String.toCString yytext, "'\n"]);
		    continue()
      end
fun yyQ170 (strm, lastMatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ171 (strm, lastMatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ172 (strm, lastMatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ173 (strm, lastMatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ174 (strm, lastMatch) = yyAction16(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
fun yyQ179 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction11(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction11(strm, yyNO_MATCH)
                      else yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction11(strm, yyNO_MATCH)
                  else yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction11(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ175 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction11(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction11(strm, yyNO_MATCH)
                      else yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction11(strm, yyNO_MATCH)
                  else yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction11(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ181 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ179(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ179(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ179(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ179(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ180 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx47
              then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < 0wx47
              then if inp = 0wx3A
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction11(strm, yyNO_MATCH)
                      else yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction11(strm, yyNO_MATCH)
                else if inp = 0wx46
                  then yyQ181(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction11(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ176 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx50
              then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < 0wx50
              then if inp = 0wx3A
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction11(strm, yyNO_MATCH)
                      else yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction11(strm, yyNO_MATCH)
                else if inp = 0wx4F
                  then yyQ180(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction11(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ179(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ49(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
and yyQ50 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ49(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ177 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ49(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ178 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ49(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3C
              then yyQ174(strm', lastMatch)
            else if inp < 0wx3C
              then if inp = 0wx20
                  then yyQ178(strm', lastMatch)
                else if inp < 0wx20
                  then if inp = 0wxD
                      then yyQ177(strm', lastMatch)
                    else if inp < 0wxD
                      then if inp <= 0wx8
                          then yyQ174(strm', lastMatch)
                          else yyQ178(strm', lastMatch)
                      else yyQ174(strm', lastMatch)
                else if inp = 0wx2D
                  then yyQ174(strm', lastMatch)
                else if inp < 0wx2D
                  then if inp = 0wx2C
                      then yyQ170(strm', lastMatch)
                      else yyQ174(strm', lastMatch)
                else if inp = 0wx3B
                  then yyQ171(strm', lastMatch)
                  else yyQ174(strm', lastMatch)
            else if inp = 0wx45
              then yyQ176(strm', lastMatch)
            else if inp < 0wx45
              then if inp = 0wx3F
                  then yyQ174(strm', lastMatch)
                else if inp < 0wx3F
                  then if inp = 0wx3D
                      then yyQ172(strm', lastMatch)
                      else yyQ173(strm', lastMatch)
                else if inp <= 0wx40
                  then yyQ174(strm', lastMatch)
                  else yyQ175(strm', lastMatch)
            else if inp = 0wx61
              then yyQ175(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ175(strm', lastMatch)
                  else yyQ174(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ175(strm', lastMatch)
              else yyQ174(strm', lastMatch)
      (* end case *))
fun yyQ107 (strm, lastMatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ108 (strm, lastMatch) = yyAction23(strm, yyNO_MATCH)
fun yyQ109 (strm, lastMatch) = yyAction24(strm, yyNO_MATCH)
fun yyQ110 (strm, lastMatch) = yyAction25(strm, yyNO_MATCH)
fun yyQ111 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ112 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ113 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ114 (strm, lastMatch) = yyAction29(strm, yyNO_MATCH)
fun yyQ115 (strm, lastMatch) = yyAction30(strm, yyNO_MATCH)
fun yyQ169 (strm, lastMatch) = yyAction54(strm, yyNO_MATCH)
fun yyQ116 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ169(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch) = yyAction32(strm, yyNO_MATCH)
fun yyQ118 (strm, lastMatch) = yyAction33(strm, yyNO_MATCH)
fun yyQ119 (strm, lastMatch) = yyAction34(strm, yyNO_MATCH)
fun yyQ120 (strm, lastMatch) = yyAction35(strm, yyNO_MATCH)
fun yyQ121 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ122 (strm, lastMatch) = yyAction42(strm, yyNO_MATCH)
fun yyQ123 (strm, lastMatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ168 (strm, lastMatch) = yyAction45(strm, yyNO_MATCH)
fun yyQ124 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ168(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ125 (strm, lastMatch) = yyAction46(strm, yyNO_MATCH)
fun yyQ126 (strm, lastMatch) = yyAction47(strm, yyNO_MATCH)
fun yyQ127 (strm, lastMatch) = yyAction72(strm, yyNO_MATCH)
fun yyQ90 (strm, lastMatch) = yyAction73(strm, yyNO_MATCH)
fun yyQ73 (strm, lastMatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ74 (strm, lastMatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ87 (strm, lastMatch) = yyAction50(strm, yyNO_MATCH)
fun yyQ86 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ87(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ87(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ87(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ87(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ87(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ87(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ85 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ86(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ86(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ86(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ86(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ86(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ86(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ84 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ85(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ85(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ85(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ85(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ85(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ85(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ75 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ84(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ84(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction49(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ84(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                  else yyAction49(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ84(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ84(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                  else yyAction49(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ84(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ84(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ84(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ84(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ84(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ84(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ84(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ82 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ83(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ83(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ83(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ83(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ83(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ83(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ81 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ82(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ82(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ82(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ82(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ82(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ82(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ80 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ81(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ81(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ81(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ81(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ81(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ81(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ76 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ80(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ80(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction49(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ80(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                  else yyAction49(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ80(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ80(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                  else yyAction49(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ80(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch) = yyAction51(strm, yyNO_MATCH)
fun yyQ79 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ74(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ74(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ78 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ79(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction51(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ79(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ128 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction72(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx56
              then yyQ74(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp < 0wx56
              then if inp = 0wx30
                  then yyQ78(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx22
                      then yyQ73(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                      else yyQ77(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ74(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ78(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                      else yyQ77(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                else if inp = 0wx55
                  then yyQ76(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                  else yyQ74(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ74(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5C
                  then yyQ73(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                else if inp < 0wx5C
                  then if inp = 0wx5B
                      then yyQ77(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                      else yyQ74(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                  else yyQ77(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp = 0wx76
              then yyQ74(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ75(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                  else yyQ74(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ74(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
              else yyQ77(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
      (* end case *))
fun yyQ167 (strm, lastMatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ166 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ167(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ165 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ166(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ164 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx46
              then yyQ165(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ163 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx4F
              then yyQ164(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ162 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx45
              then yyQ163(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ129 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3C
              then yyQ162(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ161 (strm, lastMatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ160 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ161(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ159 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ160(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ158 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ159(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ157 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ158(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ156 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ157(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ135 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx68
              then yyQ156(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ155 (strm, lastMatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ154 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ155(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ136 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ154(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ153 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ152 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ153(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ151 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ152(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ150 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ151(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ149 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ150(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ137 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ149(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ148 (strm, lastMatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ147 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ148(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ146 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyQ147(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ138 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ146(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ145 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ144 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx67
              then yyQ145(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ139 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ144(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ143 (strm, lastMatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ142 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
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
fun yyQ140 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ141(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ130 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction72(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ136(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp = 0wx63
                  then yyQ135(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                else if inp < 0wx63
                  then if inp = 0wx61
                      then yyQ139(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                      else yyAction72(strm, yyNO_MATCH)
                else if inp = 0wx64
                  then yyQ140(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                  else yyAction72(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyAction72(strm, yyNO_MATCH)
            else if inp < 0wx6F
              then if inp = 0wx6D
                  then yyAction72(strm, yyNO_MATCH)
                  else yyQ138(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp = 0wx73
              then yyQ137(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
              else yyAction72(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ14 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx46
              then yyQ15(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ131 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction72(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx4F
              then yyQ14(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
              else yyAction72(strm, yyNO_MATCH)
      (* end case *))
fun yyQ132 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ49(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ133 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ49(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ134 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ49(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2E
              then yyQ108(strm', lastMatch)
            else if inp < 0wx2E
              then if inp = 0wx24
                  then yyQ109(strm', lastMatch)
                else if inp < 0wx24
                  then if inp = 0wxE
                      then yyQ127(strm', lastMatch)
                    else if inp < 0wxE
                      then if inp = 0wxA
                          then yyQ134(strm', lastMatch)
                        else if inp < 0wxA
                          then if inp = 0wx9
                              then yyQ133(strm', lastMatch)
                              else yyQ127(strm', lastMatch)
                        else if inp = 0wxD
                          then yyQ132(strm', lastMatch)
                          else yyQ133(strm', lastMatch)
                    else if inp = 0wx21
                      then yyQ127(strm', lastMatch)
                    else if inp < 0wx21
                      then if inp = 0wx20
                          then yyQ133(strm', lastMatch)
                          else yyQ127(strm', lastMatch)
                    else if inp = 0wx22
                      then yyQ125(strm', lastMatch)
                      else yyQ127(strm', lastMatch)
                else if inp = 0wx29
                  then yyQ117(strm', lastMatch)
                else if inp < 0wx29
                  then if inp = 0wx27
                      then yyQ127(strm', lastMatch)
                    else if inp < 0wx27
                      then if inp = 0wx25
                          then yyQ130(strm', lastMatch)
                          else yyQ111(strm', lastMatch)
                      else yyQ116(strm', lastMatch)
                else if inp = 0wx2C
                  then yyQ122(strm', lastMatch)
                else if inp < 0wx2C
                  then if inp = 0wx2A
                      then yyQ112(strm', lastMatch)
                      else yyQ110(strm', lastMatch)
                  else yyQ127(strm', lastMatch)
            else if inp = 0wx5B
              then yyQ118(strm', lastMatch)
            else if inp < 0wx5B
              then if inp = 0wx3E
                  then yyQ121(strm', lastMatch)
                else if inp < 0wx3E
                  then if inp = 0wx3B
                      then yyQ115(strm', lastMatch)
                    else if inp < 0wx3B
                      then if inp = 0wx2F
                          then yyQ123(strm', lastMatch)
                          else yyQ127(strm', lastMatch)
                    else if inp = 0wx3C
                      then yyQ129(strm', lastMatch)
                      else yyQ124(strm', lastMatch)
                else if inp = 0wx45
                  then yyQ131(strm', lastMatch)
                else if inp < 0wx45
                  then if inp = 0wx3F
                      then yyQ113(strm', lastMatch)
                      else yyQ127(strm', lastMatch)
                  else yyQ127(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ120(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx5E
                  then yyQ126(strm', lastMatch)
                else if inp < 0wx5E
                  then if inp = 0wx5C
                      then yyQ128(strm', lastMatch)
                      else yyQ119(strm', lastMatch)
                  else yyQ127(strm', lastMatch)
            else if inp = 0wx7E
              then yyQ114(strm', lastMatch)
            else if inp < 0wx7E
              then if inp = 0wx7C
                  then yyQ107(strm', lastMatch)
                  else yyQ90(strm', lastMatch)
              else yyQ127(strm', lastMatch)
      (* end case *))
fun yyQ98 (strm, lastMatch) = yyAction69(strm, yyNO_MATCH)
fun yyQ99 (strm, lastMatch) = yyAction70(strm, yyNO_MATCH)
fun yyQ100 (strm, lastMatch) = yyAction71(strm, yyNO_MATCH)
fun yyQ106 (strm, lastMatch) = yyAction70(strm, yyNO_MATCH)
fun yyQ101 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction70(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ106(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
              else yyAction70(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction71(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx56
              then yyQ74(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
            else if inp < 0wx56
              then if inp = 0wx30
                  then yyQ78(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx22
                      then yyQ73(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                      else yyQ77(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ74(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ78(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                      else yyQ77(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                else if inp = 0wx55
                  then yyQ76(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                  else yyQ74(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ74(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5C
                  then yyQ73(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                else if inp < 0wx5C
                  then if inp = 0wx5B
                      then yyQ77(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                      else yyQ74(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                  else yyQ77(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
            else if inp = 0wx76
              then yyQ74(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ75(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                  else yyQ74(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ74(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
              else yyQ77(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
      (* end case *))
fun yyQ105 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ104 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx46
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ103 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction71(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx4F
              then yyQ104(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
              else yyAction71(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyQ98(strm', lastMatch)
            else if inp < 0wx22
              then if inp = 0wxB
                  then yyQ100(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ99(strm', lastMatch)
                      else yyQ100(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ101(strm', lastMatch)
                  else yyQ100(strm', lastMatch)
            else if inp = 0wx46
              then yyQ100(strm', lastMatch)
            else if inp < 0wx46
              then if inp = 0wx45
                  then yyQ103(strm', lastMatch)
                  else yyQ100(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ102(strm', lastMatch)
              else yyQ100(strm', lastMatch)
      (* end case *))
fun yyQ88 (strm, lastMatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ89 (strm, lastMatch) = yyAction39(strm, yyNO_MATCH)
fun yyQ97 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ97(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction38(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ97(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ97(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction38(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ97(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction37(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction37(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction37(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction37(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ94(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ94(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ94(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ94(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx47
              then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx47
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp = 0wx46
                  then yyQ96(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction37(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx50
              then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx50
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp = 0wx4F
                  then yyQ95(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction37(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ94(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx45
              then yyQ93(strm', lastMatch)
            else if inp < 0wx45
              then if inp = 0wx30
                  then yyQ91(strm', lastMatch)
                else if inp < 0wx30
                  then if inp = 0wx2C
                      then yyQ89(strm', lastMatch)
                      else yyQ90(strm', lastMatch)
                else if inp = 0wx3A
                  then yyQ90(strm', lastMatch)
                else if inp < 0wx3A
                  then yyQ91(strm', lastMatch)
                else if inp <= 0wx40
                  then yyQ90(strm', lastMatch)
                  else yyQ92(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ90(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx5B
                  then yyQ90(strm', lastMatch)
                else if inp < 0wx5B
                  then yyQ92(strm', lastMatch)
                else if inp <= 0wx60
                  then yyQ90(strm', lastMatch)
                  else yyQ92(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ88(strm', lastMatch)
              else yyQ90(strm', lastMatch)
      (* end case *))
fun yyQ64 (strm, lastMatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ65 (strm, lastMatch) = yyAction47(strm, yyNO_MATCH)
fun yyQ66 (strm, lastMatch) = yyAction48(strm, yyNO_MATCH)
fun yyQ67 (strm, lastMatch) = yyAction52(strm, yyNO_MATCH)
fun yyQ68 (strm, lastMatch) = yyAction53(strm, yyNO_MATCH)
fun yyQ69 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction73(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx56
              then yyQ74(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
            else if inp < 0wx56
              then if inp = 0wx30
                  then yyQ78(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx22
                      then yyQ73(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                      else yyQ77(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ74(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ78(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                      else yyQ77(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                else if inp = 0wx55
                  then yyQ76(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                  else yyQ74(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ74(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5C
                  then yyQ73(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                else if inp < 0wx5C
                  then if inp = 0wx5B
                      then yyQ77(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                      else yyQ74(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                  else yyQ77(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
            else if inp = 0wx76
              then yyQ74(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ75(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                  else yyQ74(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ74(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
              else yyQ77(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
      (* end case *))
fun yyQ70 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx4F
              then yyQ14(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ71 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ72(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2E
              then yyQ68(strm', lastMatch)
            else if inp < 0wx2E
              then if inp = 0wxD
                  then yyQ71(strm', lastMatch)
                else if inp < 0wxD
                  then if inp = 0wxA
                      then yyQ64(strm', lastMatch)
                      else yyQ68(strm', lastMatch)
                else if inp = 0wx2D
                  then yyQ66(strm', lastMatch)
                  else yyQ68(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ69(strm', lastMatch)
            else if inp < 0wx5C
              then if inp = 0wx45
                  then yyQ70(strm', lastMatch)
                  else yyQ68(strm', lastMatch)
            else if inp = 0wx5E
              then yyQ65(strm', lastMatch)
            else if inp = 0wx5D
              then yyQ67(strm', lastMatch)
              else yyQ68(strm', lastMatch)
      (* end case *))
fun yyQ40 (strm, lastMatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ41 (strm, lastMatch) = yyAction21(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
fun yyQ59 (strm, lastMatch) = yyAction18(strm, yyNO_MATCH)
fun yyQ60 (strm, lastMatch) = yyAction19(strm, yyNO_MATCH)
fun yyQ58 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx38
              then yyQ60(strm', lastMatch)
            else if inp < 0wx38
              then if inp = 0wx37
                  then yyQ59(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ63 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx49
              then yyQ58(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ62 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx49
              then yyQ63(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ61 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx43
              then yyQ62(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ42 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx53
              then yyQ61(strm', yyMATCH(strm, yyAction21, yyMATCH(strm, yyAction73, yyNO_MATCH)))
              else yyAction21(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
      (* end case *))
fun yyQ57 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ58(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ56 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ57(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ55 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx63
              then yyQ56(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ55(strm', yyMATCH(strm, yyAction21, yyMATCH(strm, yyAction73, yyNO_MATCH)))
              else yyAction21(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
      (* end case *))
fun yyQ53 (strm, lastMatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ52 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx38
              then yyQ53(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ54 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx46
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx54
              then yyQ54(strm', yyMATCH(strm, yyAction21, yyMATCH(strm, yyAction73, yyNO_MATCH)))
              else yyAction21(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
      (* end case *))
fun yyQ51 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ45 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ51(strm', yyMATCH(strm, yyAction21, yyMATCH(strm, yyAction73, yyNO_MATCH)))
              else yyAction21(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
      (* end case *))
fun yyQ46 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx4F
              then yyQ14(strm', yyMATCH(strm, yyAction21, yyMATCH(strm, yyAction73, yyNO_MATCH)))
              else yyAction21(strm, yyMATCH(strm, yyAction73, yyNO_MATCH))
      (* end case *))
fun yyQ47 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ49(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ49(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ50(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx42
              then yyQ41(strm', lastMatch)
            else if inp < 0wx42
              then if inp = 0wx20
                  then yyQ48(strm', lastMatch)
                else if inp < 0wx20
                  then if inp = 0wxD
                      then yyQ47(strm', lastMatch)
                    else if inp < 0wxD
                      then if inp <= 0wx8
                          then yyQ41(strm', lastMatch)
                          else yyQ48(strm', lastMatch)
                      else yyQ41(strm', lastMatch)
                else if inp = 0wx3C
                  then yyQ41(strm', lastMatch)
                else if inp < 0wx3C
                  then if inp = 0wx3B
                      then yyQ40(strm', lastMatch)
                      else yyQ41(strm', lastMatch)
                else if inp = 0wx41
                  then yyQ42(strm', lastMatch)
                  else yyQ41(strm', lastMatch)
            else if inp = 0wx56
              then yyQ41(strm', lastMatch)
            else if inp < 0wx56
              then if inp = 0wx46
                  then yyQ41(strm', lastMatch)
                else if inp < 0wx46
                  then if inp = 0wx45
                      then yyQ46(strm', lastMatch)
                      else yyQ41(strm', lastMatch)
                else if inp = 0wx55
                  then yyQ44(strm', lastMatch)
                  else yyQ41(strm', lastMatch)
            else if inp = 0wx62
              then yyQ41(strm', lastMatch)
            else if inp < 0wx62
              then if inp = 0wx61
                  then yyQ43(strm', lastMatch)
                  else yyQ41(strm', lastMatch)
            else if inp = 0wx75
              then yyQ45(strm', lastMatch)
              else yyQ41(strm', lastMatch)
      (* end case *))
fun yyQ28 (strm, lastMatch) = yyAction63(strm, yyNO_MATCH)
fun yyQ29 (strm, lastMatch) = yyAction64(strm, yyNO_MATCH)
fun yyQ38 (strm, lastMatch) = yyAction66(strm, yyNO_MATCH)
fun yyQ39 (strm, lastMatch) = yyAction67(strm, yyNO_MATCH)
fun yyQ30 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyAction65(strm, yyNO_MATCH)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ39(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = 0wx5C
              then yyQ38(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction68(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction68(strm, yyNO_MATCH)
                      else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction68(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction68(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction68(strm, yyNO_MATCH)
              else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
      (* end case *))
fun yyQ31 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction68(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction68(strm, yyNO_MATCH)
                      else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction68(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction68(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction68(strm, yyNO_MATCH)
              else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
      (* end case *))
fun yyQ37 (strm, lastMatch) = yyAction64(strm, yyNO_MATCH)
fun yyQ32 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ37(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ34(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ34(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ34(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ34(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction2(strm, yyNO_MATCH)
              else yyQ34(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ35 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction68(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyAction68(strm, yyNO_MATCH)
            else if inp < 0wx22
              then if inp = 0wxB
                  then yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction68(strm, yyNO_MATCH)
                      else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction68(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp = 0wx47
              then yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp < 0wx47
              then if inp = 0wx46
                  then yyQ36(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                  else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction68(strm, yyNO_MATCH)
              else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
      (* end case *))
fun yyQ33 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction68(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyAction68(strm, yyNO_MATCH)
            else if inp < 0wx22
              then if inp = 0wxB
                  then yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction68(strm, yyNO_MATCH)
                      else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction68(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp = 0wx50
              then yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp < 0wx50
              then if inp = 0wx4F
                  then yyQ35(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                  else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction68(strm, yyNO_MATCH)
              else yyQ34(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
      (* end case *))
fun yyQ2 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyQ28(strm', lastMatch)
            else if inp < 0wx22
              then if inp = 0wxB
                  then yyQ31(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ29(strm', lastMatch)
                      else yyQ31(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ32(strm', lastMatch)
                  else yyQ31(strm', lastMatch)
            else if inp = 0wx46
              then yyQ31(strm', lastMatch)
            else if inp < 0wx46
              then if inp = 0wx45
                  then yyQ33(strm', lastMatch)
                  else yyQ31(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ30(strm', lastMatch)
              else yyQ31(strm', lastMatch)
      (* end case *))
fun yyQ19 (strm, lastMatch) = yyAction60(strm, yyNO_MATCH)
fun yyQ20 (strm, lastMatch) = yyAction61(strm, yyNO_MATCH)
fun yyQ24 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction62(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp = 0wx28
              then yyAction62(strm, yyNO_MATCH)
            else if inp < 0wx28
              then yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp <= 0wx29
              then yyAction62(strm, yyNO_MATCH)
              else yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
      (* end case *))
fun yyQ21 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction62(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp = 0wx28
              then yyAction62(strm, yyNO_MATCH)
            else if inp < 0wx28
              then yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp <= 0wx29
              then yyAction62(strm, yyNO_MATCH)
              else yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
      (* end case *))
fun yyQ27 (strm, lastMatch) = yyAction55(strm, yyNO_MATCH)
fun yyQ22 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ27(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
              else yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ24(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx28
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx28
              then yyQ24(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp <= 0wx29
              then yyAction2(strm, yyNO_MATCH)
              else yyQ24(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ25 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyAction62(strm, yyNO_MATCH)
            else if inp < 0wx28
              then if inp = 0wx22
                  then yyAction62(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp = 0wx46
              then yyQ26(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp < 0wx46
              then if inp <= 0wx29
                  then yyAction62(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
              else yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
      (* end case *))
fun yyQ23 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyAction62(strm, yyNO_MATCH)
            else if inp < 0wx28
              then if inp = 0wx22
                  then yyAction62(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp = 0wx4F
              then yyQ25(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp < 0wx4F
              then if inp <= 0wx29
                  then yyAction62(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
              else yyQ24(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
      (* end case *))
fun yyQ1 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx29
              then yyQ19(strm', lastMatch)
            else if inp < 0wx29
              then if inp = 0wx23
                  then yyQ21(strm', lastMatch)
                else if inp < 0wx23
                  then if inp = 0wx22
                      then yyQ20(strm', lastMatch)
                      else yyQ21(strm', lastMatch)
                else if inp = 0wx28
                  then yyQ22(strm', lastMatch)
                  else yyQ21(strm', lastMatch)
            else if inp = 0wx45
              then yyQ23(strm', lastMatch)
              else yyQ21(strm', lastMatch)
      (* end case *))
fun yyQ9 (strm, lastMatch) = yyAction58(strm, yyNO_MATCH)
fun yyQ18 (strm, lastMatch) = yyAction58(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ18(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
              else yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch) = yyAction57(strm, yyNO_MATCH)
fun yyQ11 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx29
              then yyQ17(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
              else yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch) = yyAction56(strm, yyNO_MATCH)
fun yyQ12 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ16(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
              else yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx4F
              then yyQ14(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
              else yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx29
              then yyQ9(strm', lastMatch)
            else if inp < 0wx29
              then if inp = 0wxE
                  then yyQ9(strm', lastMatch)
                else if inp < 0wxE
                  then if inp = 0wxD
                      then yyQ10(strm', lastMatch)
                      else yyQ9(strm', lastMatch)
                else if inp = 0wx28
                  then yyQ12(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = 0wx45
              then yyQ13(strm', lastMatch)
            else if inp < 0wx45
              then if inp = 0wx2A
                  then yyQ11(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
              else yyQ9(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COM => yyQ0(!(yystrm), yyNO_MATCH)
    | CODE => yyQ1(!(yystrm), yyNO_MATCH)
    | STRING => yyQ2(!(yystrm), yyNO_MATCH)
    | CHARSET => yyQ3(!(yystrm), yyNO_MATCH)
    | CHARCLASS => yyQ4(!(yystrm), yyNO_MATCH)
    | CURLY => yyQ5(!(yystrm), yyNO_MATCH)
    | RESTRING => yyQ6(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ7(!(yystrm), yyNO_MATCH)
    | DIRECTIVE => yyQ8(!(yystrm), yyNO_MATCH)
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
