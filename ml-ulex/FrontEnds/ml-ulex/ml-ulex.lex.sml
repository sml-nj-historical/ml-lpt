structure MLULexLex  = struct

    datatype yystart_state = 
COM | CODE | STRING | CHARSET | CHARCLASS | CURLY | RESTRING | INITIAL | DIRECTIVE
    structure UserDeclarations = 
      struct

 
  structure Tok = MLULexTokens

  val comLvl : int ref = ref 0		(* nesting depth of comments *)
  val comStart : int ref = ref 0	(* start line of current comment *)

  type lex_result = Tok.token
  fun eof() = Tok.EOF

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

  fun hexVal ss = 
        Substring.foldl 
	  (fn (dig, acc) => (Word32.fromInt o hexDigit) dig + 0w16 * acc) 
	  0w0 ss



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
fun yyAction1 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.KW_defs)
fun yyAction2 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_name)
fun yyAction3 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_states)
fun yyAction4 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_let)
fun yyAction5 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CHARSET; Tok.KW_charset)
fun yyAction6 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction7 (strm, lastMatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction8 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.SEMI)
fun yyAction9 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.EQ)
fun yyAction10 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.GT)
fun yyAction11 (strm, lastMatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm;  YYBEGIN INITIAL; REJECT()
      end
fun yyAction12 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.UTF8)
fun yyAction13 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.ASCII7)
fun yyAction14 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.ASCII8)
fun yyAction15 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.SEMI)
fun yyAction16 (strm, lastMatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm;  YYBEGIN INITIAL; REJECT()
      end
fun yyAction17 (strm, lastMatch) = (yystrm := strm;  Tok.BAR)
fun yyAction18 (strm, lastMatch) = (yystrm := strm;  Tok.DOT)
fun yyAction19 (strm, lastMatch) = (yystrm := strm;  Tok.DOLLAR)
fun yyAction20 (strm, lastMatch) = (yystrm := strm;  Tok.PLUS)
fun yyAction21 (strm, lastMatch) = (yystrm := strm;  Tok.AMP)
fun yyAction22 (strm, lastMatch) = (yystrm := strm;  Tok.STAR)
fun yyAction23 (strm, lastMatch) = (yystrm := strm;  Tok.QUERY)
fun yyAction24 (strm, lastMatch) = (yystrm := strm;  Tok.SEMI)
fun yyAction25 (strm, lastMatch) = (yystrm := strm;  Tok.LP)
fun yyAction26 (strm, lastMatch) = (yystrm := strm;  Tok.RP)
fun yyAction27 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CHARCLASS; Tok.LSB)
fun yyAction28 (strm, lastMatch) = (yystrm := strm;  Tok.RSB)
fun yyAction29 (strm, lastMatch) = (yystrm := strm;  YYBEGIN CURLY; Tok.LCB)
fun yyAction30 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.RCB)
fun yyAction31 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction32 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.INT (valOf (Int.fromString yytext))
      end
fun yyAction33 (strm, lastMatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction34 (strm, lastMatch) = (yystrm := strm;  YYBEGIN DIRECTIVE; Tok.LT)
fun yyAction35 (strm, lastMatch) = (yystrm := strm;  Tok.GT)
fun yyAction36 (strm, lastMatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction37 (strm, lastMatch) = (yystrm := strm;  Tok.SLASH)
fun yyAction38 (strm, lastMatch) = (yystrm := strm;  Tok.EQ)
fun yyAction39 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.DARROW)
fun yyAction40 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN RESTRING; continue())
fun yyAction41 (strm, lastMatch) = (yystrm := strm;  Tok.CARAT)
fun yyAction42 (strm, lastMatch) = (yystrm := strm;  Tok.DASH)
fun yyAction43 (strm, lastMatch) = let
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
fun yyAction44 (strm, lastMatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;  Tok.UCHAR (hexVal (Substring.triml 2 yysubstr))
      end
fun yyAction45 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.RSB)
fun yyAction46 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  Tok.UCHAR (hd yyunicode)
      end
fun yyAction47 (strm, lastMatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue()
      end
fun yyAction48 (strm, lastMatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    addText yytext;
	    ignore(continue() before YYBEGIN CODE);
	    continue()
      end
fun yyAction49 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; comLvl := !comLvl+1; continue()
      end
fun yyAction50 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue()
      end
fun yyAction51 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction52 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         if !pcount = 0 then () else addText yytext;
		    inc pcount; continue()
      end
fun yyAction53 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL; Tok.CODE (getText()))
		    else (addText yytext; continue())
      end
fun yyAction54 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    addText "\""; continue()
      end
fun yyAction55 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction56 (strm, lastMatch) = (yystrm := strm;  Tok.BOGUS)
fun yyAction57 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; print ("unclosed string\n");
 	            Tok.BOGUS
      end
fun yyAction58 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction59 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction60 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction61 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction62 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN INITIAL; continue())
fun yyAction63 (strm, lastMatch) = (yystrm := strm;
       print ("unclosed string\n"); continue())
fun yyAction64 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  Tok.UCHAR (hd yyunicode)
      end
fun yyAction65 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  Tok.UCHAR (hd yyunicode)
      end
fun yyAction66 (strm, lastMatch) = let
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
fun yyQ139 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ140 (strm, lastMatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ141 (strm, lastMatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ142 (strm, lastMatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ143 (strm, lastMatch) = yyAction11(strm, yyMATCH(strm, yyAction66, yyNO_MATCH))
fun yyQ147 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ147(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ147(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ147(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ147(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ144 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ147(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ147(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ147(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ147(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
and yyQ40 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ145 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ146 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ140(strm', lastMatch)
            else if inp < 0wx3B
              then if inp = 0wx20
                  then yyQ146(strm', lastMatch)
                else if inp < 0wx20
                  then if inp = 0wxD
                      then yyQ145(strm', lastMatch)
                    else if inp < 0wxD
                      then if inp <= 0wx8
                          then yyQ143(strm', lastMatch)
                          else yyQ146(strm', lastMatch)
                      else yyQ143(strm', lastMatch)
                else if inp = 0wx2C
                  then yyQ139(strm', lastMatch)
                  else yyQ143(strm', lastMatch)
            else if inp = 0wx41
              then yyQ144(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx3E
                  then yyQ142(strm', lastMatch)
                else if inp < 0wx3E
                  then if inp = 0wx3C
                      then yyQ143(strm', lastMatch)
                      else yyQ141(strm', lastMatch)
                  else yyQ143(strm', lastMatch)
            else if inp = 0wx61
              then yyQ144(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ144(strm', lastMatch)
                  else yyQ143(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ144(strm', lastMatch)
              else yyQ143(strm', lastMatch)
      (* end case *))
fun yyQ88 (strm, lastMatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ89 (strm, lastMatch) = yyAction18(strm, yyNO_MATCH)
fun yyQ90 (strm, lastMatch) = yyAction19(strm, yyNO_MATCH)
fun yyQ91 (strm, lastMatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ92 (strm, lastMatch) = yyAction21(strm, yyNO_MATCH)
fun yyQ93 (strm, lastMatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ94 (strm, lastMatch) = yyAction23(strm, yyNO_MATCH)
fun yyQ95 (strm, lastMatch) = yyAction24(strm, yyNO_MATCH)
fun yyQ138 (strm, lastMatch) = yyAction47(strm, yyNO_MATCH)
fun yyQ96 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ138(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ98 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ99 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ100 (strm, lastMatch) = yyAction29(strm, yyNO_MATCH)
fun yyQ101 (strm, lastMatch) = yyAction34(strm, yyNO_MATCH)
fun yyQ102 (strm, lastMatch) = yyAction35(strm, yyNO_MATCH)
fun yyQ103 (strm, lastMatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ104 (strm, lastMatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ137 (strm, lastMatch) = yyAction39(strm, yyNO_MATCH)
fun yyQ105 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ137(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ106 (strm, lastMatch) = yyAction40(strm, yyNO_MATCH)
fun yyQ107 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ108 (strm, lastMatch) = yyAction65(strm, yyNO_MATCH)
fun yyQ77 (strm, lastMatch) = yyAction66(strm, yyNO_MATCH)
fun yyQ62 (strm, lastMatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ74 (strm, lastMatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ73 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ74(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ74(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ74(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ74(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ74(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ74(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ72 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ73(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ73(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ73(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ73(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ73(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ73(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ71 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ72(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ72(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ72(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ72(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ72(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ72(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ63 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ71(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ71(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction43(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ71(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ71(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ71(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ71(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ71(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ71(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ71(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ71(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ71(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ71(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ69 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ70(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ70(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ70(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ70(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ70(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ70(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ68 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ69(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ69(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ69(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ69(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ69(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ69(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ67 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ68(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ68(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ68(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ68(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ68(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ68(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ64 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ67(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ67(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction43(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ67(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ67(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ67(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ67(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ62(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ62(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ65 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ66(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ66(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ109 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx56
              then yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < 0wx56
              then if inp = 0wx30
                  then yyQ65(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx22
                      then yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ65(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                else if inp = 0wx55
                  then yyQ64(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5C
                  then yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < 0wx5C
                  then if inp = 0wx5B
                      then yyAction65(strm, yyNO_MATCH)
                      else yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ63(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ136 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ135 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ136(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ134 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ135(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ133 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ134(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ132 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ133(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ131 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ132(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ113 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx68
              then yyQ131(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ130 (strm, lastMatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ129 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ130(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ114 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ129(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ128 (strm, lastMatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ127 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ128(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ126 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ127(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ125 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ126(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ124 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ125(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ115 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ124(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ123 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ122 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ123(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ121 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyQ122(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ116 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ121(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ120 (strm, lastMatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ119 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ120(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ118 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
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
fun yyQ110 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyAction65(strm, yyNO_MATCH)
            else if inp < 0wx6D
              then if inp = 0wx64
                  then yyQ117(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < 0wx64
                  then if inp = 0wx63
                      then yyQ113(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                else if inp = 0wx6C
                  then yyQ114(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ115(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx6E
                  then yyQ116(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ108(strm', lastMatch)
            else if inp < 0wx2D
              then if inp = 0wx23
                  then yyQ108(strm', lastMatch)
                else if inp < 0wx23
                  then if inp = 0wxD
                      then yyQ111(strm', lastMatch)
                    else if inp < 0wxD
                      then if inp = 0wxA
                          then yyQ61(strm', lastMatch)
                        else if inp < 0wxA
                          then if inp = 0wx9
                              then yyQ112(strm', lastMatch)
                              else yyQ108(strm', lastMatch)
                          else yyQ112(strm', lastMatch)
                    else if inp = 0wx21
                      then yyQ108(strm', lastMatch)
                    else if inp < 0wx21
                      then if inp = 0wx20
                          then yyQ112(strm', lastMatch)
                          else yyQ108(strm', lastMatch)
                      else yyQ106(strm', lastMatch)
                else if inp = 0wx28
                  then yyQ96(strm', lastMatch)
                else if inp < 0wx28
                  then if inp = 0wx26
                      then yyQ92(strm', lastMatch)
                    else if inp < 0wx26
                      then if inp = 0wx24
                          then yyQ90(strm', lastMatch)
                          else yyQ110(strm', lastMatch)
                      else yyQ108(strm', lastMatch)
                else if inp = 0wx2B
                  then yyQ91(strm', lastMatch)
                else if inp < 0wx2B
                  then if inp = 0wx29
                      then yyQ97(strm', lastMatch)
                      else yyQ93(strm', lastMatch)
                  else yyQ103(strm', lastMatch)
            else if inp = 0wx5B
              then yyQ98(strm', lastMatch)
            else if inp < 0wx5B
              then if inp = 0wx3C
                  then yyQ101(strm', lastMatch)
                else if inp < 0wx3C
                  then if inp = 0wx30
                      then yyQ108(strm', lastMatch)
                    else if inp < 0wx30
                      then if inp = 0wx2E
                          then yyQ89(strm', lastMatch)
                          else yyQ104(strm', lastMatch)
                    else if inp = 0wx3B
                      then yyQ95(strm', lastMatch)
                      else yyQ108(strm', lastMatch)
                else if inp = 0wx3F
                  then yyQ94(strm', lastMatch)
                else if inp < 0wx3F
                  then if inp = 0wx3D
                      then yyQ105(strm', lastMatch)
                      else yyQ102(strm', lastMatch)
                  else yyQ108(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ100(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx5E
                  then yyQ107(strm', lastMatch)
                else if inp < 0wx5E
                  then if inp = 0wx5C
                      then yyQ109(strm', lastMatch)
                      else yyQ99(strm', lastMatch)
                  else yyQ108(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ77(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ88(strm', lastMatch)
              else yyQ108(strm', lastMatch)
      (* end case *))
fun yyQ82 (strm, lastMatch) = yyAction62(strm, yyNO_MATCH)
fun yyQ83 (strm, lastMatch) = yyAction63(strm, yyNO_MATCH)
fun yyQ84 (strm, lastMatch) = yyAction64(strm, yyNO_MATCH)
fun yyQ87 (strm, lastMatch) = yyAction63(strm, yyNO_MATCH)
fun yyQ85 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ87(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx56
              then yyQ62(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < 0wx56
              then if inp = 0wx30
                  then yyQ65(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx22
                      then yyQ62(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ62(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ65(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = 0wx55
                  then yyQ64(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyQ62(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ62(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5C
                  then yyQ62(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < 0wx5C
                  then if inp = 0wx5B
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ62(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ62(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ63(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyQ62(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ62(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ84(strm', lastMatch)
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ84(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ83(strm', lastMatch)
                      else yyQ84(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ85(strm', lastMatch)
                  else yyQ84(strm', lastMatch)
            else if inp = 0wx23
              then yyQ84(strm', lastMatch)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ82(strm', lastMatch)
                  else yyQ84(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ86(strm', lastMatch)
              else yyQ84(strm', lastMatch)
      (* end case *))
fun yyQ75 (strm, lastMatch) = yyAction30(strm, yyNO_MATCH)
fun yyQ76 (strm, lastMatch) = yyAction33(strm, yyNO_MATCH)
fun yyQ81 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ81(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction32(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ81(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ81(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction32(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ81(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ80(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ80(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ80(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ80(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ80(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ80(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ80(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ80(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ79(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx2D
                  then yyQ77(strm', lastMatch)
                else if inp < 0wx2D
                  then if inp = 0wx2C
                      then yyQ76(strm', lastMatch)
                      else yyQ77(strm', lastMatch)
                else if inp = 0wx30
                  then yyQ78(strm', lastMatch)
                else if inp < 0wx30
                  then yyQ77(strm', lastMatch)
                else if inp <= 0wx39
                  then yyQ78(strm', lastMatch)
                  else yyQ77(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ77(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx5B
                  then yyQ77(strm', lastMatch)
                else if inp < 0wx5B
                  then yyQ79(strm', lastMatch)
                else if inp <= 0wx60
                  then yyQ77(strm', lastMatch)
                  else yyQ79(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ75(strm', lastMatch)
              else yyQ77(strm', lastMatch)
      (* end case *))
fun yyQ54 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ55 (strm, lastMatch) = yyAction42(strm, yyNO_MATCH)
fun yyQ56 (strm, lastMatch) = yyAction45(strm, yyNO_MATCH)
fun yyQ57 (strm, lastMatch) = yyAction46(strm, yyNO_MATCH)
fun yyQ58 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx56
              then yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx56
              then if inp = 0wx30
                  then yyQ65(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx22
                      then yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                      else yyAction66(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ65(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                      else yyAction66(strm, yyNO_MATCH)
                else if inp = 0wx55
                  then yyQ64(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5C
                  then yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < 0wx5C
                  then if inp = 0wx5B
                      then yyAction66(strm, yyNO_MATCH)
                      else yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ63(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx21
              then yyQ57(strm', lastMatch)
            else if inp < 0wx21
              then if inp = 0wxB
                  then yyQ60(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wx9
                      then yyQ60(strm', lastMatch)
                    else if inp = 0wxA
                      then yyQ61(strm', lastMatch)
                      else yyQ57(strm', lastMatch)
                else if inp = 0wxE
                  then yyQ57(strm', lastMatch)
                else if inp < 0wxE
                  then if inp = 0wxD
                      then yyQ59(strm', lastMatch)
                      else yyQ60(strm', lastMatch)
                else if inp = 0wx20
                  then yyQ60(strm', lastMatch)
                  else yyQ57(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ58(strm', lastMatch)
            else if inp < 0wx5C
              then if inp = 0wx2D
                  then yyQ55(strm', lastMatch)
                  else yyQ57(strm', lastMatch)
            else if inp = 0wx5E
              then yyQ54(strm', lastMatch)
            else if inp = 0wx5D
              then yyQ56(strm', lastMatch)
              else yyQ57(strm', lastMatch)
      (* end case *))
fun yyQ31 (strm, lastMatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ32 (strm, lastMatch) = yyAction16(strm, yyMATCH(strm, yyAction66, yyNO_MATCH))
fun yyQ49 (strm, lastMatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ50 (strm, lastMatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ48 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx38
              then yyQ50(strm', lastMatch)
            else if inp < 0wx38
              then if inp = 0wx37
                  then yyQ49(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ53 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx49
              then yyQ48(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ52 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx49
              then yyQ53(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ51 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx43
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction66, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx53
              then yyQ51(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction66, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction66, yyNO_MATCH))
      (* end case *))
fun yyQ47 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ48(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ46 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ47(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ45 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx63
              then yyQ46(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ34 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction66, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ45(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction66, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction66, yyNO_MATCH))
      (* end case *))
fun yyQ43 (strm, lastMatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ42 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx38
              then yyQ43(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx46
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ35 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction66, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx54
              then yyQ44(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction66, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction66, yyNO_MATCH))
      (* end case *))
fun yyQ41 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ36 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction66, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ41(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction66, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction66, yyNO_MATCH))
      (* end case *))
fun yyQ37 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ33(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx20
                  then yyQ38(strm', lastMatch)
                else if inp < 0wx20
                  then if inp = 0wxD
                      then yyQ37(strm', lastMatch)
                    else if inp < 0wxD
                      then if inp <= 0wx8
                          then yyQ32(strm', lastMatch)
                          else yyQ38(strm', lastMatch)
                      else yyQ32(strm', lastMatch)
                else if inp = 0wx3B
                  then yyQ31(strm', lastMatch)
                  else yyQ32(strm', lastMatch)
            else if inp = 0wx61
              then yyQ34(strm', lastMatch)
            else if inp < 0wx61
              then if inp = 0wx55
                  then yyQ35(strm', lastMatch)
                  else yyQ32(strm', lastMatch)
            else if inp = 0wx75
              then yyQ36(strm', lastMatch)
              else yyQ32(strm', lastMatch)
      (* end case *))
fun yyQ22 (strm, lastMatch) = yyAction56(strm, yyNO_MATCH)
fun yyQ23 (strm, lastMatch) = yyAction57(strm, yyNO_MATCH)
fun yyQ29 (strm, lastMatch) = yyAction59(strm, yyNO_MATCH)
fun yyQ30 (strm, lastMatch) = yyAction60(strm, yyNO_MATCH)
fun yyQ24 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyAction58(strm, yyNO_MATCH)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ30(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
                  else yyAction58(strm, yyNO_MATCH)
            else if inp = 0wx5C
              then yyQ29(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
              else yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction61(strm, yyNO_MATCH)
                      else yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction61(strm, yyNO_MATCH)
                  else yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction61(strm, yyNO_MATCH)
                  else yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction61(strm, yyNO_MATCH)
              else yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
      (* end case *))
fun yyQ25 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction61(strm, yyNO_MATCH)
                      else yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction61(strm, yyNO_MATCH)
                  else yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction61(strm, yyNO_MATCH)
                  else yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction61(strm, yyNO_MATCH)
              else yyQ28(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
      (* end case *))
fun yyQ27 (strm, lastMatch) = yyAction57(strm, yyNO_MATCH)
fun yyQ26 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ27(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
              else yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ25(strm', lastMatch)
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ25(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ23(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ26(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = 0wx23
              then yyQ25(strm', lastMatch)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ22(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ24(strm', lastMatch)
              else yyQ25(strm', lastMatch)
      (* end case *))
fun yyQ16 (strm, lastMatch) = yyAction53(strm, yyNO_MATCH)
fun yyQ17 (strm, lastMatch) = yyAction54(strm, yyNO_MATCH)
fun yyQ21 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ21(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction55(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp = 0wx28
              then yyAction55(strm, yyNO_MATCH)
            else if inp < 0wx28
              then yyQ21(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp <= 0wx29
              then yyAction55(strm, yyNO_MATCH)
              else yyQ21(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
      (* end case *))
fun yyQ18 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ21(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction55(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp = 0wx28
              then yyAction55(strm, yyNO_MATCH)
            else if inp < 0wx28
              then yyQ21(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp <= 0wx29
              then yyAction55(strm, yyNO_MATCH)
              else yyQ21(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
      (* end case *))
fun yyQ20 (strm, lastMatch) = yyAction48(strm, yyNO_MATCH)
fun yyQ19 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ20(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyQ19(strm', lastMatch)
            else if inp < 0wx28
              then if inp = 0wx22
                  then yyQ17(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx29
              then yyQ16(strm', lastMatch)
              else yyQ18(strm', lastMatch)
      (* end case *))
fun yyQ9 (strm, lastMatch) = yyAction51(strm, yyNO_MATCH)
fun yyQ15 (strm, lastMatch) = yyAction51(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ15(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch) = yyAction50(strm, yyNO_MATCH)
fun yyQ11 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx29
              then yyQ14(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ12 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ13(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyQ12(strm', lastMatch)
            else if inp < 0wx28
              then if inp = 0wxD
                  then yyQ10(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = 0wx2A
              then yyQ11(strm', lastMatch)
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
