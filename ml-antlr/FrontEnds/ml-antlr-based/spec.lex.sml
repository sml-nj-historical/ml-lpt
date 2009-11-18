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
Vector.fromList[([(0w0,0w12,5),
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
(0w124,0w124,77)], []), ([], [39, 68]), ([(0w10,0w10,11)], [39, 68]), ([(0w42,0w42,10)], [39, 68]), ([(0w41,0w41,9)], [39, 68]), ([], [38]), ([], [37]), ([], [39]), ([(0w0,0w33,17),
(0w35,0w39,17),
(0w42,0w2147483647,17)], [43, 68]), ([], [42, 68]), ([(0w42,0w42,16)], [40, 68]), ([], [41, 68]), ([], [36]), ([(0w0,0w33,17),
(0w35,0w39,17),
(0w42,0w2147483647,17)], [43]), ([(0w0,0w9,26),
(0w11,0w12,26),
(0w14,0w33,26),
(0w35,0w91,26),
(0w93,0w2147483647,26)], [48, 68]), ([], [45, 68]), ([(0w10,0w10,25)], [45, 68]), ([], [44, 68]), ([(0w34,0w34,23),
(0w92,0w92,24)], [46, 68]), ([], [49]), ([], [47]), ([], [45]), ([(0w0,0w9,26),
(0w11,0w12,26),
(0w14,0w33,26),
(0w35,0w91,26),
(0w93,0w2147483647,26)], [48]), ([], [68]), ([], [50, 68]), ([(0w10,0w10,55)], [50, 68]), ([], [66, 68]), ([(0w39,0w39,54),
(0w48,0w57,54),
(0w65,0w90,54),
(0w95,0w95,54),
(0w97,0w122,54)], [53, 68]), ([(0w42,0w42,53)], [61, 68]), ([], [62, 68]), ([], [57, 68]), ([], [59, 68]), ([(0w62,0w62,52)], [68]), ([], [58, 68]), ([], [60, 68]), ([], [67, 68]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w122,46),
(0w46,0w46,47)], [52, 68]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w104,46),
(0w106,0w122,46),
(0w46,0w46,47),
(0w105,0w105,49)], [52, 68]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w101,46),
(0w103,0w122,46),
(0w46,0w46,47),
(0w102,0w102,48)], [52, 68]), ([], [63, 68]), ([], [56, 68]), ([], [64, 68]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w122,46),
(0w46,0w46,47)], [52]), ([], [54]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w122,46),
(0w46,0w46,47)], [51, 52]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w102,46),
(0w104,0w122,46),
(0w46,0w46,47),
(0w103,0w103,50)], [52]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w104,46),
(0w106,0w122,46),
(0w46,0w46,47),
(0w105,0w105,51)], [52]), ([(0w39,0w39,46),
(0w48,0w57,46),
(0w65,0w90,46),
(0w95,0w95,46),
(0w97,0w115,46),
(0w117,0w122,46),
(0w46,0w46,47),
(0w116,0w116,51)], [52, 55]), ([], [65]), ([], [35]), ([(0w39,0w39,54),
(0w48,0w57,54),
(0w65,0w90,54),
(0w95,0w95,54),
(0w97,0w122,54)], [53]), ([], [50]), ([(0w9,0w12,155),
(0w32,0w32,155),
(0w13,0w13,156)], [1, 68]), ([(0w9,0w12,155),
(0w32,0w32,155),
(0w13,0w13,156)], [1, 68]), ([], [33, 68]), ([], [18, 68]), ([(0w100,0w100,83),
(0w101,0w101,84),
(0w104,0w104,85),
(0w105,0w105,86),
(0w107,0w107,87),
(0w110,0w110,88),
(0w114,0w114,89),
(0w115,0w115,90),
(0w116,0w116,91),
(0w119,0w119,92)], [68]), ([(0w42,0w42,82)], [25, 68]), ([], [26, 68]), ([], [20, 68]), ([], [19, 68]), ([], [24, 68]), ([(0w62,0w62,81)], [68]), ([], [29, 68]), ([], [22, 68]), ([], [23, 68]), ([(0w62,0w62,80)], [30, 68]), ([], [21, 68]), ([], [17, 68]), ([(0w39,0w39,78),
(0w48,0w57,78),
(0w65,0w90,78),
(0w95,0w95,78),
(0w97,0w122,78)], [2, 68]), ([], [27, 68]), ([], [28, 68]), ([(0w39,0w39,78),
(0w48,0w57,78),
(0w65,0w90,78),
(0w95,0w95,78),
(0w97,0w101,78),
(0w103,0w122,78),
(0w102,0w102,79)], [2, 68]), ([], [16, 68]), ([(0w39,0w39,78),
(0w48,0w57,78),
(0w65,0w90,78),
(0w95,0w95,78),
(0w97,0w122,78)], [2]), ([(0w39,0w39,78),
(0w48,0w57,78),
(0w65,0w90,78),
(0w95,0w95,78),
(0w97,0w122,78)], [0, 2]), ([], [32]), ([], [31]), ([], [34]), ([(0w101,0w101,145),
(0w114,0w114,146)], []), ([(0w110,0w110,141)], []), ([(0w101,0w101,136)], []), ([(0w109,0w109,131)], []), ([(0w101,0w101,124)], []), ([(0w97,0w97,114),
(0w111,0w111,115)], []), ([(0w101,0w101,108)], []), ([(0w116,0w116,104)], []), ([(0w111,0w111,97),
(0w114,0w114,98)], []), ([(0w104,0w104,93)], []), ([(0w101,0w101,94)], []), ([(0w114,0w114,95)], []), ([(0w101,0w101,96)], []), ([], [12]), ([(0w107,0w107,100)], []), ([(0w121,0w121,99)], []), ([], [11]), ([(0w101,0w101,101)], []), ([(0w110,0w110,102)], []), ([(0w115,0w115,103)], [3]), ([], [3]), ([(0w97,0w97,105)], []), ([(0w114,0w114,106)], []), ([(0w116,0w116,107)], []), ([], [9]), ([(0w102,0w102,109)], []), ([(0w99,0w99,110)], []), ([(0w101,0w101,111)], []), ([(0w108,0w108,112)], []), ([(0w108,0w108,113)], []), ([], [14]), ([(0w109,0w109,122)], []), ([(0w110,0w110,116)], []), ([(0w116,0w116,117)], []), ([(0w101,0w101,118)], []), ([(0w114,0w114,119)], []), ([(0w109,0w109,120)], []), ([(0w115,0w115,121)], [6]), ([], [6]), ([(0w101,0w101,123)], []), ([], [8]), ([(0w121,0w121,125)], []), ([(0w119,0w119,126)], []), ([(0w111,0w111,127)], []), ([(0w114,0w114,128)], []), ([(0w100,0w100,129)], []), ([(0w115,0w115,130)], [5]), ([], [5]), ([(0w112,0w112,132)], []), ([(0w111,0w111,133)], []), ([(0w114,0w114,134)], []), ([(0w116,0w116,135)], []), ([], [7]), ([(0w97,0w97,137)], []), ([(0w100,0w100,138)], []), ([(0w101,0w101,139)], []), ([(0w114,0w114,140)], []), ([], [15]), ([(0w116,0w116,142)], []), ([(0w114,0w114,143)], []), ([(0w121,0w121,144)], []), ([], [10]), ([(0w102,0w102,153)], []), ([(0w111,0w111,147)], []), ([(0w112,0w112,148)], []), ([(0w112,0w112,149)], []), ([(0w105,0w105,150)], []), ([(0w110,0w110,151)], []), ([(0w103,0w103,152)], []), ([], [13]), ([(0w115,0w115,154)], []), ([], [4]), ([(0w9,0w12,155),
(0w32,0w32,155),
(0w13,0w13,156)], [1]), ([(0w9,0w12,155),
(0w32,0w32,155),
(0w13,0w13,156)], [1])]
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
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.KW_header)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BAR)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.AT)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.DOLLAR)
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.PLUS)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.STAR)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.QUERY)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COLON)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SEMI)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LP)
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RP)
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LSB)
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RSB)
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SLASH)
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.EQ)
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.ARROW)
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.DARROW)
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN INITIAL);
		    Tok.STRING (getText())
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue()
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN CONSTR);
	    continue()
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN CODE);
	    continue()
      end
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
       comLvl := !comLvl+1; continue())
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
       comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue())
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;  continue())
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         if !pcount = 0 then () else addText yytext;
		    inc pcount; continue()
      end
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL;
		       Tok.CODE (getText()))
		    else (addText yytext; continue())
      end
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    continue()
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; Tok.BOGUS
      end
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; err (!yylineno, !yycolno, "unclosed string");
 	            Tok.BOGUS
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
fun yyAction49 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;  continue())
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.OF)
fun yyAction52 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.TYVAR yytext
      end
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.IDDOT yytext
      end
fun yyAction55 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.INT yytext
      end
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BAR)
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.STAR)
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COLON)
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.SEMI)
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LP)
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RP)
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LCB)
fun yyAction64 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RCB)
fun yyAction65 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.ARROW)
fun yyAction66 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN CONSTR);
		    Tok.STRING (getText())
      end
fun yyAction67 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.EQ)
fun yyAction68 (strm, lastMatch : yymatch) = let
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
  yyAction65, yyAction66, yyAction67, yyAction68])
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
