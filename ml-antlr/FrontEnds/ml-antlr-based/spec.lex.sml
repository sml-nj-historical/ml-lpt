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
#[([(0w0,0w12,5),
(0w14,0w39,5),
(0w41,0w41,5),
(0w43,0w2147483647,5),
(0w13,0w13,6),
(0w40,0w40,7),
(0w42,0w42,8)], []), ([(0w0,0w33,12),
(0w35,0w39,12),
(0w42,0w2147483647,12),
(0w34,0w34,13),
(0w40,0w40,14),
(0w41,0w41,15)], []), ([(0w0,0w9,18),
(0w11,0w12,18),
(0w14,0w33,18),
(0w35,0w91,18),
(0w93,0w2147483647,18),
(0w10,0w10,19),
(0w13,0w13,20),
(0w34,0w34,21),
(0w92,0w92,22)], []), ([(0w0,0w8,27),
(0w14,0w31,27),
(0w33,0w33,27),
(0w35,0w38,27),
(0w43,0w43,27),
(0w46,0w57,27),
(0w60,0w60,27),
(0w62,0w64,27),
(0w91,0w96,27),
(0w126,0w2147483647,27),
(0w9,0w12,28),
(0w32,0w32,28),
(0w13,0w13,29),
(0w34,0w34,30),
(0w39,0w39,31),
(0w40,0w40,32),
(0w41,0w41,33),
(0w42,0w42,34),
(0w44,0w44,35),
(0w45,0w45,36),
(0w58,0w58,37),
(0w59,0w59,38),
(0w61,0w61,39),
(0w65,0w90,40),
(0w97,0w99,40),
(0w101,0w110,40),
(0w112,0w122,40),
(0w100,0w100,41),
(0w111,0w111,42),
(0w123,0w123,43),
(0w124,0w124,44),
(0w125,0w125,45)], []), ([(0w0,0w8,27),
(0w14,0w31,27),
(0w33,0w33,27),
(0w35,0w35,27),
(0w38,0w39,27),
(0w46,0w46,27),
(0w48,0w57,27),
(0w60,0w60,27),
(0w62,0w62,27),
(0w92,0w92,27),
(0w94,0w96,27),
(0w123,0w123,27),
(0w125,0w2147483647,27),
(0w9,0w12,56),
(0w32,0w32,56),
(0w13,0w13,57),
(0w34,0w34,58),
(0w36,0w36,59),
(0w37,0w37,60),
(0w40,0w40,61),
(0w41,0w41,62),
(0w42,0w42,63),
(0w43,0w43,64),
(0w44,0w44,65),
(0w45,0w45,66),
(0w47,0w47,67),
(0w58,0w58,68),
(0w59,0w59,69),
(0w61,0w61,70),
(0w63,0w63,71),
(0w64,0w64,72),
(0w65,0w90,73),
(0w97,0w110,73),
(0w112,0w122,73),
(0w91,0w91,74),
(0w93,0w93,75),
(0w111,0w111,76),
(0w124,0w124,77)], []), ([], [38, 67]), ([(0w10,0w10,11)], [38, 67]), ([(0w42,0w42,10)], [38, 67]), ([(0w41,0w41,9)], [38, 67]), ([], [37]), ([], [36]), ([], [38]), ([(0w0,0w33,17),
(0w35,0w39,17),
(0w42,0w2147483647,17)], [42, 67]), ([], [41, 67]), ([(0w42,0w42,16)], [39, 67]), ([], [40, 67]), ([], [35]), ([(0w0,0w33,17),
(0w35,0w39,17),
(0w42,0w2147483647,17)], [42]), ([(0w0,0w9,26),
(0w11,0w12,26),
(0w14,0w33,26),
(0w35,0w91,26),
(0w93,0w2147483647,26)], [47, 67]), ([], [44, 67]), ([(0w10,0w10,25)], [44, 67]), ([], [43, 67]), ([(0w34,0w34,23),
(0w92,0w92,24)], [45, 67]), ([], [48]), ([], [46]), ([], [44]), ([(0w0,0w9,26),
(0w11,0w12,26),
(0w14,0w33,26),
(0w35,0w91,26),
(0w93,0w2147483647,26)], [47]), ([], [67]), ([], [49, 67]), ([(0w10,0w10,55)], [49, 67]), ([], [65, 67]), ([(0w39,0w39,54),
(0w48,0w57,54),
(0w65,0w90,54),
(0w95,0w95,54),
(0w97,0w122,54)], [52, 67]), ([(0w42,0w42,53)], [60, 67]), ([], [61, 67]), ([], [56, 67]), ([], [58, 67]), ([(0w62,0w62,52)], [67]), ([], [57, 67]), ([], [59, 67]), ([], [66, 67]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w122,46),
(0w46,0w46,47)], [51, 67]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w104,46),
(0w106,0w122,46),
(0w46,0w46,47),
(0w105,0w105,49)], [51, 67]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w101,46),
(0w103,0w122,46),
(0w46,0w46,47),
(0w102,0w102,48)], [51, 67]), ([], [62, 67]), ([], [55, 67]), ([], [63, 67]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w122,46),
(0w46,0w46,47)], [51]), ([], [53]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w122,46),
(0w46,0w46,47)], [50, 51]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w102,46),
(0w104,0w122,46),
(0w46,0w46,47),
(0w103,0w103,50)], [51]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w104,46),
(0w106,0w122,46),
(0w46,0w46,47),
(0w105,0w105,51)], [51]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w115,46),
(0w117,0w122,46),
(0w46,0w46,47),
(0w116,0w116,51)], [51, 54]), ([], [64]), ([], [34]), ([(0w39,0w39,54),
(0w48,0w57,54),
(0w65,0w90,54),
(0w95,0w95,54),
(0w97,0w122,54)], [52]), ([], [49]), ([(0w9,0w12,149),
(0w32,0w32,149),
(0w13,0w13,150)], [1, 67]), ([(0w9,0w12,149),
(0w32,0w32,149),
(0w13,0w13,150)], [1, 67]), ([], [32, 67]), ([], [17, 67]), ([(0w100,0w100,83),
(0w101,0w101,84),
(0w105,0w105,85),
(0w107,0w107,86),
(0w110,0w110,87),
(0w114,0w114,88),
(0w115,0w115,89),
(0w116,0w116,90),
(0w119,0w119,91)], [67]), ([(0w42,0w42,82)], [24, 67]), ([], [25, 67]), ([], [19, 67]), ([], [18, 67]), ([], [23, 67]), ([(0w62,0w62,81)], [67]), ([], [28, 67]), ([], [21, 67]), ([], [22, 67]), ([(0w62,0w62,80)], [29, 67]), ([], [20, 67]), ([], [16, 67]), ([(0w39,0w39,78),
(0w48,0w57,78),
(0w65,0w90,78),
(0w95,0w95,78),
(0w97,0w122,78)], [2, 67]), ([], [26, 67]), ([], [27, 67]), ([(0w39,0w39,78),
(0w48,0w57,78),
(0w65,0w90,78),
(0w95,0w95,78),
(0w97,0w101,78),
(0w103,0w122,78),
(0w102,0w102,79)], [2, 67]), ([], [15, 67]), ([(0w39,0w39,78),
(0w48,0w57,78),
(0w65,0w90,78),
(0w95,0w95,78),
(0w97,0w122,78)], [2]), ([(0w39,0w39,78),
(0w48,0w57,78),
(0w65,0w90,78),
(0w95,0w95,78),
(0w97,0w122,78)], [0, 2]), ([], [31]), ([], [30]), ([], [33]), ([(0w101,0w101,139),
(0w114,0w114,140)], []), ([(0w110,0w110,135)], []), ([(0w109,0w109,130)], []), ([(0w101,0w101,123)], []), ([(0w97,0w97,113),
(0w111,0w111,114)], []), ([(0w101,0w101,107)], []), ([(0w116,0w116,103)], []), ([(0w111,0w111,96),
(0w114,0w114,97)], []), ([(0w104,0w104,92)], []), ([(0w101,0w101,93)], []), ([(0w114,0w114,94)], []), ([(0w101,0w101,95)], []), ([], [12]), ([(0w107,0w107,99)], []), ([(0w121,0w121,98)], []), ([], [11]), ([(0w101,0w101,100)], []), ([(0w110,0w110,101)], []), ([(0w115,0w115,102)], [3]), ([], [3]), ([(0w97,0w97,104)], []), ([(0w114,0w114,105)], []), ([(0w116,0w116,106)], []), ([], [9]), ([(0w102,0w102,108)], []), ([(0w99,0w99,109)], []), ([(0w101,0w101,110)], []), ([(0w108,0w108,111)], []), ([(0w108,0w108,112)], []), ([], [14]), ([(0w109,0w109,121)], []), ([(0w110,0w110,115)], []), ([(0w116,0w116,116)], []), ([(0w101,0w101,117)], []), ([(0w114,0w114,118)], []), ([(0w109,0w109,119)], []), ([(0w115,0w115,120)], [6]), ([], [6]), ([(0w101,0w101,122)], []), ([], [8]), ([(0w121,0w121,124)], []), ([(0w119,0w119,125)], []), ([(0w111,0w111,126)], []), ([(0w114,0w114,127)], []), ([(0w100,0w100,128)], []), ([(0w115,0w115,129)], [5]), ([], [5]), ([(0w112,0w112,131)], []), ([(0w111,0w111,132)], []), ([(0w114,0w114,133)], []), ([(0w116,0w116,134)], []), ([], [7]), ([(0w116,0w116,136)], []), ([(0w114,0w114,137)], []), ([(0w121,0w121,138)], []), ([], [10]), ([(0w102,0w102,147)], []), ([(0w111,0w111,141)], []), ([(0w112,0w112,142)], []), ([(0w112,0w112,143)], []), ([(0w105,0w105,144)], []), ([(0w110,0w110,145)], []), ([(0w103,0w103,146)], []), ([], [13]), ([(0w115,0w115,148)], []), ([], [4]), ([(0w9,0w12,149),
(0w32,0w32,149),
(0w13,0w13,150)], [1]), ([(0w9,0w12,149),
(0w32,0w32,149),
(0w13,0w13,150)], [1])]
    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
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
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
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
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
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
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.SEMI)
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LP)
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RP)
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LCB)
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RCB)
fun yyAction64 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.ARROW)
fun yyAction65 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN CONSTR);
		    Tok.STRING (getText())
      end
fun yyAction66 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.EQ)
fun yyAction67 (strm, lastMatch : yymatch) = let
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
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48, yyAction49, yyAction50, yyAction51, yyAction52,
  yyAction53, yyAction54, yyAction55, yyAction56, yyAction57, yyAction58,
  yyAction59, yyAction60, yyAction61, yyAction62, yyAction63, yyAction64,
  yyAction65, yyAction66, yyAction67])
in
  if ULexBuffer.eof(!(yystrm))
    then let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        (case (!(yyss))
         of _ => (UserDeclarations.eof())
        (* end case *))
      end
    else (case (!(yyss))
       of COM => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
        | CODE => yygo yyactTable (1, !(yystrm), yyNO_MATCH)
        | STRING => yygo yyactTable (2, !(yystrm), yyNO_MATCH)
        | CONSTR => yygo yyactTable (3, !(yystrm), yyNO_MATCH)
        | INITIAL => yygo yyactTable (4, !(yystrm), yyNO_MATCH)
      (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
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

    fun lex sm 
(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
(yystrm, ss, sm)
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
		 lex sm 
(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end
