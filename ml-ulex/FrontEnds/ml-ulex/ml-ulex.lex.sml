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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.KW_defs)
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_name)
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_states)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_let)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CHARSET; Tok.KW_charset)
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.SEMI)
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.EQ)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.GT)
fun yyAction11 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm;  YYBEGIN INITIAL; REJECT()
      end
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.UTF8)
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.ASCII7)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.ASCII8)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.SEMI)
fun yyAction16 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm;  YYBEGIN INITIAL; REJECT()
      end
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BAR)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.DOT)
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.DOLLAR)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.PLUS)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.AMP)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.STAR)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.QUERY)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SEMI)
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LP)
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RP)
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CHARCLASS; Tok.LSB)
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RSB)
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CURLY; Tok.LCB)
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.RCB)
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.INT (valOf (Int.fromString yytext))
      end
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.LT)
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.GT)
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SLASH)
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.EQ)
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.DARROW)
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN RESTRING; continue())
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.CARAT)
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.DASH)
fun yyAction43 (strm, lastMatch : yymatch) = let
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
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;  Tok.UCHAR (hexVal (Substring.triml 2 yysubstr))
      end
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.CHAR (String.sub (yytext, 1))
      end
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.RSB)
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  mkUChar yyunicode
      end
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue()
      end
fun yyAction49 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    addText yytext;
	    ignore(continue() before YYBEGIN CODE);
	    continue()
      end
fun yyAction50 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; comLvl := !comLvl+1; continue()
      end
fun yyAction51 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue()
      end
fun yyAction52 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         if !pcount = 0 then () else addText yytext;
		    inc pcount; continue()
      end
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL; Tok.CODE (getText()))
		    else (addText yytext; continue())
      end
fun yyAction55 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    addText "\""; continue()
      end
fun yyAction56 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BOGUS)
fun yyAction58 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; print ("unclosed string\n");
 	            Tok.BOGUS
      end
fun yyAction59 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction60 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction61 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction62 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; continue())
fun yyAction64 (strm, lastMatch : yymatch) = (yystrm := strm;
       print ("unclosed string\n"); continue())
fun yyAction65 (strm, lastMatch : yymatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  mkUChar yyunicode
      end
fun yyAction66 (strm, lastMatch : yymatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  mkUChar yyunicode
      end
fun yyAction67 (strm, lastMatch : yymatch) = let
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
fun yyQ149 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ149(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ149(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ149(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ149(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ148 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ149(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ149(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ149(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ149(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ147 (strm, lastMatch : yymatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ146 (strm, lastMatch : yymatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ145 (strm, lastMatch : yymatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ144 (strm, lastMatch : yymatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
and yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ143 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ142 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ141 (strm, lastMatch : yymatch) = yyAction11(strm, yyMATCH(strm, yyAction67, yyNO_MATCH))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ145(strm', lastMatch)
            else if inp < 0wx3B
              then if inp = 0wx20
                  then yyQ142(strm', lastMatch)
                else if inp < 0wx20
                  then if inp = 0wxD
                      then yyQ143(strm', lastMatch)
                    else if inp < 0wxD
                      then if inp <= 0wx8
                          then yyQ141(strm', lastMatch)
                          else yyQ142(strm', lastMatch)
                      else yyQ141(strm', lastMatch)
                else if inp = 0wx2C
                  then yyQ144(strm', lastMatch)
                  else yyQ141(strm', lastMatch)
            else if inp = 0wx41
              then yyQ148(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx3E
                  then yyQ147(strm', lastMatch)
                else if inp < 0wx3E
                  then if inp = 0wx3C
                      then yyQ141(strm', lastMatch)
                      else yyQ146(strm', lastMatch)
                  else yyQ141(strm', lastMatch)
            else if inp = 0wx61
              then yyQ148(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ148(strm', lastMatch)
                  else yyQ141(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ148(strm', lastMatch)
              else yyQ141(strm', lastMatch)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = yyAction67(strm, yyNO_MATCH)
fun yyQ114 (strm, lastMatch : yymatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ113 (strm, lastMatch : yymatch) = yyAction29(strm, yyNO_MATCH)
fun yyQ112 (strm, lastMatch : yymatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ111 (strm, lastMatch : yymatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ71 (strm, lastMatch : yymatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ68(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ68(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction43(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ68(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ68(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ68(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ68(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ75(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ75(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ75(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ75(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ75(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ75(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ72(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ72(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction43(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ72(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ72(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ72(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ72(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ65(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ65(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ76(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction45(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ76(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ62 (strm, lastMatch : yymatch) = yyAction45(strm, yyNO_MATCH)
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx56
              then yyQ65(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx56
              then if inp = 0wx30
                  then yyQ64(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx22
                      then yyQ63(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                      else yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ65(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ64(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                      else yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp = 0wx55
                  then yyQ66(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyQ65(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ65(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5C
                  then yyQ63(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < 0wx5C
                  then if inp = 0wx5B
                      then yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                      else yyQ65(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp = 0wx76
              then yyQ65(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ67(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyQ65(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ65(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyQ62(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ108 (strm, lastMatch : yymatch) = yyAction23(strm, yyNO_MATCH)
fun yyQ107 (strm, lastMatch : yymatch) = yyAction35(strm, yyNO_MATCH)
fun yyQ115 (strm, lastMatch : yymatch) = yyAction39(strm, yyNO_MATCH)
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ115(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = yyAction34(strm, yyNO_MATCH)
fun yyQ104 (strm, lastMatch : yymatch) = yyAction24(strm, yyNO_MATCH)
fun yyQ103 (strm, lastMatch : yymatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ102 (strm, lastMatch : yymatch) = yyAction18(strm, yyNO_MATCH)
fun yyQ101 (strm, lastMatch : yymatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ100 (strm, lastMatch : yymatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ99 (strm, lastMatch : yymatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ98 (strm, lastMatch : yymatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ116 (strm, lastMatch : yymatch) = yyAction48(strm, yyNO_MATCH)
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ116(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = yyAction21(strm, yyNO_MATCH)
fun yyQ126 (strm, lastMatch : yymatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ125 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ126(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ124 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ125(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ124(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ122 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ123(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ122(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ129 (strm, lastMatch : yymatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ128 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ129(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ127 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyQ128(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ127(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ131 (strm, lastMatch : yymatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ130 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ131(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ130(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ134 (strm, lastMatch : yymatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ133 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ134(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ132 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ133(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ132(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ140 (strm, lastMatch : yymatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ139 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ140(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ138 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ139(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ137 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ138(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ136 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ137(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ135 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ136(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx68
              then yyQ135(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyAction66(strm, yyNO_MATCH)
            else if inp < 0wx6D
              then if inp = 0wx64
                  then yyQ118(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < 0wx64
                  then if inp = 0wx63
                      then yyQ117(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                      else yyAction66(strm, yyNO_MATCH)
                else if inp = 0wx6C
                  then yyQ119(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ121(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx6E
                  then yyQ120(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = yyAction19(strm, yyNO_MATCH)
fun yyQ93 (strm, lastMatch : yymatch) = yyAction40(strm, yyNO_MATCH)
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = yyAction66(strm, yyNO_MATCH)
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ90(strm', lastMatch)
            else if inp < 0wx2D
              then if inp = 0wx23
                  then yyQ90(strm', lastMatch)
                else if inp < 0wx23
                  then if inp = 0wxD
                      then yyQ92(strm', lastMatch)
                    else if inp < 0wxD
                      then if inp = 0wxA
                          then yyQ56(strm', lastMatch)
                        else if inp < 0wxA
                          then if inp = 0wx9
                              then yyQ91(strm', lastMatch)
                              else yyQ90(strm', lastMatch)
                          else yyQ91(strm', lastMatch)
                    else if inp = 0wx21
                      then yyQ90(strm', lastMatch)
                    else if inp < 0wx21
                      then if inp = 0wx20
                          then yyQ91(strm', lastMatch)
                          else yyQ90(strm', lastMatch)
                      else yyQ93(strm', lastMatch)
                else if inp = 0wx28
                  then yyQ97(strm', lastMatch)
                else if inp < 0wx28
                  then if inp = 0wx26
                      then yyQ96(strm', lastMatch)
                    else if inp < 0wx26
                      then if inp = 0wx24
                          then yyQ94(strm', lastMatch)
                          else yyQ95(strm', lastMatch)
                      else yyQ90(strm', lastMatch)
                else if inp = 0wx2B
                  then yyQ100(strm', lastMatch)
                else if inp < 0wx2B
                  then if inp = 0wx29
                      then yyQ98(strm', lastMatch)
                      else yyQ99(strm', lastMatch)
                  else yyQ101(strm', lastMatch)
            else if inp = 0wx5B
              then yyQ109(strm', lastMatch)
            else if inp < 0wx5B
              then if inp = 0wx3C
                  then yyQ105(strm', lastMatch)
                else if inp < 0wx3C
                  then if inp = 0wx30
                      then yyQ90(strm', lastMatch)
                    else if inp < 0wx30
                      then if inp = 0wx2E
                          then yyQ102(strm', lastMatch)
                          else yyQ103(strm', lastMatch)
                    else if inp = 0wx3B
                      then yyQ104(strm', lastMatch)
                      else yyQ90(strm', lastMatch)
                else if inp = 0wx3F
                  then yyQ108(strm', lastMatch)
                else if inp < 0wx3F
                  then if inp = 0wx3D
                      then yyQ106(strm', lastMatch)
                      else yyQ107(strm', lastMatch)
                  else yyQ90(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ113(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx5E
                  then yyQ112(strm', lastMatch)
                else if inp < 0wx5E
                  then if inp = 0wx5C
                      then yyQ110(strm', lastMatch)
                      else yyQ111(strm', lastMatch)
                  else yyQ90(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ77(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ114(strm', lastMatch)
              else yyQ90(strm', lastMatch)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx56
              then yyQ65(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < 0wx56
              then if inp = 0wx30
                  then yyQ64(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx22
                      then yyQ63(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ65(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ64(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp = 0wx55
                  then yyQ66(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyQ65(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ65(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5C
                  then yyQ63(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < 0wx5C
                  then if inp = 0wx5B
                      then yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyQ65(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp = 0wx76
              then yyQ65(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ67(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyQ65(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ65(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyQ62(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = yyAction63(strm, yyNO_MATCH)
fun yyQ89 (strm, lastMatch : yymatch) = yyAction64(strm, yyNO_MATCH)
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ89(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = yyAction64(strm, yyNO_MATCH)
fun yyQ84 (strm, lastMatch : yymatch) = yyAction65(strm, yyNO_MATCH)
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      then yyQ85(strm', lastMatch)
                      else yyQ84(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ86(strm', lastMatch)
                  else yyQ84(strm', lastMatch)
            else if inp = 0wx23
              then yyQ84(strm', lastMatch)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ87(strm', lastMatch)
                  else yyQ84(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ88(strm', lastMatch)
              else yyQ84(strm', lastMatch)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = yyAction30(strm, yyNO_MATCH)
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ82(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ82(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ82(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ82(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ82(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ82(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ82(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ82(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ83(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction32(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ83(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ83(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction32(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ83(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = yyAction33(strm, yyNO_MATCH)
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ80(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx2D
                  then yyQ77(strm', lastMatch)
                else if inp < 0wx2D
                  then if inp = 0wx2C
                      then yyQ78(strm', lastMatch)
                      else yyQ77(strm', lastMatch)
                else if inp = 0wx30
                  then yyQ79(strm', lastMatch)
                else if inp < 0wx30
                  then yyQ77(strm', lastMatch)
                else if inp <= 0wx39
                  then yyQ79(strm', lastMatch)
                  else yyQ77(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ77(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx5B
                  then yyQ77(strm', lastMatch)
                else if inp < 0wx5B
                  then yyQ80(strm', lastMatch)
                else if inp <= 0wx60
                  then yyQ77(strm', lastMatch)
                  else yyQ80(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ81(strm', lastMatch)
              else yyQ77(strm', lastMatch)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ60 (strm, lastMatch : yymatch) = yyAction46(strm, yyNO_MATCH)
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction67(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx56
              then yyQ65(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
            else if inp < 0wx56
              then if inp = 0wx30
                  then yyQ64(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx22
                      then yyQ63(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyQ62(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ65(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ64(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyQ62(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                else if inp = 0wx55
                  then yyQ66(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyQ65(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ65(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5C
                  then yyQ63(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                else if inp < 0wx5C
                  then if inp = 0wx5B
                      then yyQ62(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyQ65(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyQ62(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
            else if inp = 0wx76
              then yyQ65(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ67(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyQ65(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ65(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
              else yyQ62(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = yyAction42(strm, yyNO_MATCH)
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = yyAction47(strm, yyNO_MATCH)
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx21
              then yyQ54(strm', lastMatch)
            else if inp < 0wx21
              then if inp = 0wxB
                  then yyQ55(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wx9
                      then yyQ55(strm', lastMatch)
                    else if inp = 0wxA
                      then yyQ56(strm', lastMatch)
                      else yyQ54(strm', lastMatch)
                else if inp = 0wxE
                  then yyQ54(strm', lastMatch)
                else if inp < 0wxE
                  then if inp = 0wxD
                      then yyQ57(strm', lastMatch)
                      else yyQ55(strm', lastMatch)
                else if inp = 0wx20
                  then yyQ55(strm', lastMatch)
                  else yyQ54(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ59(strm', lastMatch)
            else if inp < 0wx5C
              then if inp = 0wx2D
                  then yyQ58(strm', lastMatch)
                  else yyQ54(strm', lastMatch)
            else if inp = 0wx5E
              then yyQ61(strm', lastMatch)
            else if inp = 0wx5D
              then yyQ60(strm', lastMatch)
              else yyQ54(strm', lastMatch)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx38
              then yyQ41(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ40(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction67, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ39(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction67, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction67, yyNO_MATCH))
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ46 (strm, lastMatch : yymatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx38
              then yyQ47(strm', lastMatch)
            else if inp < 0wx38
              then if inp = 0wx37
                  then yyQ46(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ45(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ44(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx63
              then yyQ43(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction67, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ42(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction67, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction67, yyNO_MATCH))
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx46
              then yyQ40(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction67, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx54
              then yyQ48(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction67, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction67, yyNO_MATCH))
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx49
              then yyQ45(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx49
              then yyQ51(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx43
              then yyQ50(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction67, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx53
              then yyQ49(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction67, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction67, yyNO_MATCH))
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ53(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ52(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = yyAction16(strm, yyMATCH(strm, yyAction67, yyNO_MATCH))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ35(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx20
                  then yyQ32(strm', lastMatch)
                else if inp < 0wx20
                  then if inp = 0wxD
                      then yyQ33(strm', lastMatch)
                    else if inp < 0wxD
                      then if inp <= 0wx8
                          then yyQ31(strm', lastMatch)
                          else yyQ32(strm', lastMatch)
                      else yyQ31(strm', lastMatch)
                else if inp = 0wx3B
                  then yyQ34(strm', lastMatch)
                  else yyQ31(strm', lastMatch)
            else if inp = 0wx61
              then yyQ37(strm', lastMatch)
            else if inp < 0wx61
              then if inp = 0wx55
                  then yyQ36(strm', lastMatch)
                  else yyQ31(strm', lastMatch)
            else if inp = 0wx75
              then yyQ38(strm', lastMatch)
              else yyQ31(strm', lastMatch)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = yyAction60(strm, yyNO_MATCH)
fun yyQ27 (strm, lastMatch : yymatch) = yyAction61(strm, yyNO_MATCH)
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyAction59(strm, yyNO_MATCH)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ27(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyAction59(strm, yyNO_MATCH)
            else if inp = 0wx5C
              then yyQ28(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
              else yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = yyAction57(strm, yyNO_MATCH)
fun yyQ29 (strm, lastMatch : yymatch) = yyAction58(strm, yyNO_MATCH)
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ29(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
              else yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = yyAction58(strm, yyNO_MATCH)
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction62(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction62(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction62(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction62(strm, yyNO_MATCH)
              else yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction62(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction62(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction62(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction62(strm, yyNO_MATCH)
              else yyQ30(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ22(strm', lastMatch)
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ22(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ23(strm', lastMatch)
                      else yyQ22(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ24(strm', lastMatch)
                  else yyQ22(strm', lastMatch)
            else if inp = 0wx23
              then yyQ22(strm', lastMatch)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ25(strm', lastMatch)
                  else yyQ22(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ26(strm', lastMatch)
              else yyQ22(strm', lastMatch)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = yyAction54(strm, yyNO_MATCH)
fun yyQ20 (strm, lastMatch : yymatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ20(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = yyAction55(strm, yyNO_MATCH)
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ21(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = 0wx28
              then yyAction56(strm, yyNO_MATCH)
            else if inp < 0wx28
              then yyQ21(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp <= 0wx29
              then yyAction56(strm, yyNO_MATCH)
              else yyQ21(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ21(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = 0wx28
              then yyAction56(strm, yyNO_MATCH)
            else if inp < 0wx28
              then yyQ21(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp <= 0wx29
              then yyAction56(strm, yyNO_MATCH)
              else yyQ21(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyQ18(strm', lastMatch)
            else if inp < 0wx28
              then if inp = 0wx22
                  then yyQ17(strm', lastMatch)
                  else yyQ16(strm', lastMatch)
            else if inp = 0wx29
              then yyQ19(strm', lastMatch)
              else yyQ16(strm', lastMatch)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = yyAction51(strm, yyNO_MATCH)
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx29
              then yyQ13(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = yyAction50(strm, yyNO_MATCH)
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ14(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = yyAction52(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ15(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = yyAction52(strm, yyNO_MATCH)
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyQ11(strm', lastMatch)
            else if inp < 0wx28
              then if inp = 0wxD
                  then yyQ10(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = 0wx2A
              then yyQ12(strm', lastMatch)
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
