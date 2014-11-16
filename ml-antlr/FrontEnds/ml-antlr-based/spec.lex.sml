structure SpecLex  = struct

    datatype yystart_state = 
COM | CODE | STRING | PRECODE | CONSTR | INITIAL
    local

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

val pcount = ref 0			(* nesting depth of parentheses in CODE *)
fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
#[([(0w0,0w12,6),
(0w14,0w39,6),
(0w41,0w41,6),
(0w43,0w2147483647,6),
(0w13,0w13,7),
(0w40,0w40,8),
(0w42,0w42,9)], []), ([(0w0,0w33,13),
(0w35,0w39,13),
(0w42,0w2147483647,13),
(0w34,0w34,14),
(0w40,0w40,15),
(0w41,0w41,16)], []), ([(0w0,0w9,19),
(0w11,0w12,19),
(0w14,0w33,19),
(0w35,0w91,19),
(0w93,0w2147483647,19),
(0w10,0w10,20),
(0w13,0w13,21),
(0w34,0w34,22),
(0w92,0w92,23)], []), ([(0w0,0w8,28),
(0w14,0w31,28),
(0w33,0w33,28),
(0w35,0w35,28),
(0w38,0w39,28),
(0w41,0w41,28),
(0w46,0w46,28),
(0w48,0w57,28),
(0w60,0w60,28),
(0w62,0w62,28),
(0w92,0w92,28),
(0w94,0w96,28),
(0w123,0w123,28),
(0w125,0w2147483647,28),
(0w9,0w12,29),
(0w32,0w32,29),
(0w13,0w13,30),
(0w34,0w34,31),
(0w36,0w36,32),
(0w37,0w37,33),
(0w40,0w40,34),
(0w42,0w42,35),
(0w43,0w43,36),
(0w44,0w44,37),
(0w45,0w45,38),
(0w47,0w47,39),
(0w58,0w58,40),
(0w59,0w59,41),
(0w61,0w61,42),
(0w63,0w63,43),
(0w64,0w64,44),
(0w65,0w90,45),
(0w97,0w110,45),
(0w112,0w122,45),
(0w91,0w91,46),
(0w93,0w93,47),
(0w111,0w111,48),
(0w124,0w124,49)], []), ([(0w0,0w8,28),
(0w14,0w31,28),
(0w33,0w33,28),
(0w35,0w38,28),
(0w43,0w43,28),
(0w46,0w57,28),
(0w60,0w60,28),
(0w62,0w64,28),
(0w91,0w96,28),
(0w126,0w2147483647,28),
(0w9,0w12,134),
(0w32,0w32,134),
(0w13,0w13,135),
(0w34,0w34,136),
(0w39,0w39,137),
(0w40,0w40,138),
(0w41,0w41,139),
(0w42,0w42,140),
(0w44,0w44,141),
(0w45,0w45,142),
(0w58,0w58,143),
(0w59,0w59,144),
(0w61,0w61,145),
(0w65,0w90,146),
(0w97,0w99,146),
(0w101,0w110,146),
(0w112,0w122,146),
(0w100,0w100,147),
(0w111,0w111,148),
(0w123,0w123,149),
(0w124,0w124,150),
(0w125,0w125,151)], []), ([(0w0,0w8,28),
(0w14,0w31,28),
(0w33,0w33,28),
(0w35,0w35,28),
(0w38,0w39,28),
(0w46,0w46,28),
(0w48,0w57,28),
(0w60,0w60,28),
(0w62,0w62,28),
(0w92,0w92,28),
(0w94,0w96,28),
(0w123,0w123,28),
(0w125,0w2147483647,28),
(0w9,0w12,29),
(0w32,0w32,29),
(0w13,0w13,30),
(0w34,0w34,162),
(0w36,0w36,32),
(0w37,0w37,33),
(0w40,0w40,163),
(0w41,0w41,164),
(0w42,0w42,35),
(0w43,0w43,36),
(0w44,0w44,37),
(0w45,0w45,38),
(0w47,0w47,39),
(0w58,0w58,40),
(0w59,0w59,41),
(0w61,0w61,42),
(0w63,0w63,43),
(0w64,0w64,44),
(0w65,0w90,45),
(0w97,0w110,45),
(0w112,0w122,45),
(0w91,0w91,46),
(0w93,0w93,47),
(0w111,0w111,48),
(0w124,0w124,49)], []), ([], [41, 72]), ([(0w10,0w10,12)], [41, 72]), ([(0w42,0w42,11)], [41, 72]), ([(0w41,0w41,10)], [41, 72]), ([], [40]), ([], [39]), ([], [41]), ([(0w0,0w33,18),
(0w35,0w39,18),
(0w42,0w2147483647,18)], [47, 72]), ([], [46, 72]), ([(0w42,0w42,17)], [44, 72]), ([], [45, 72]), ([], [38]), ([(0w0,0w33,18),
(0w35,0w39,18),
(0w42,0w2147483647,18)], [47]), ([(0w0,0w9,27),
(0w11,0w12,27),
(0w14,0w33,27),
(0w35,0w91,27),
(0w93,0w2147483647,27)], [52, 72]), ([], [49, 72]), ([(0w10,0w10,26)], [49, 72]), ([], [48, 72]), ([(0w34,0w34,24),
(0w92,0w92,25)], [50, 72]), ([], [53]), ([], [51]), ([], [49]), ([(0w0,0w9,27),
(0w11,0w12,27),
(0w14,0w33,27),
(0w35,0w91,27),
(0w93,0w2147483647,27)], [52]), ([], [72]), ([(0w9,0w12,132),
(0w32,0w32,132),
(0w13,0w13,133)], [1, 72]), ([(0w9,0w12,132),
(0w32,0w32,132),
(0w13,0w13,133)], [1, 72]), ([], [43, 72]), ([], [19, 72]), ([(0w100,0w100,55),
(0w101,0w101,56),
(0w104,0w104,57),
(0w105,0w105,58),
(0w107,0w107,59),
(0w110,0w110,60),
(0w114,0w114,61),
(0w115,0w115,62),
(0w116,0w116,63),
(0w118,0w118,64),
(0w119,0w119,65)], [72]), ([(0w42,0w42,54)], [42, 72]), ([], [21, 72]), ([], [20, 72]), ([], [25, 72]), ([(0w62,0w62,53)], [72]), ([], [30, 72]), ([], [23, 72]), ([], [24, 72]), ([(0w62,0w62,52)], [31, 72]), ([], [22, 72]), ([], [18, 72]), ([(0w39,0w39,50),
(0w48,0w57,50),
(0w65,0w90,50),
(0w95,0w95,50),
(0w97,0w122,50)], [2, 72]), ([], [28, 72]), ([], [29, 72]), ([(0w39,0w39,50),
(0w48,0w57,50),
(0w65,0w90,50),
(0w95,0w95,50),
(0w97,0w101,50),
(0w103,0w122,50),
(0w102,0w102,51)], [2, 72]), ([], [17, 72]), ([(0w39,0w39,50),
(0w48,0w57,50),
(0w65,0w90,50),
(0w95,0w95,50),
(0w97,0w122,50)], [2]), ([(0w39,0w39,50),
(0w48,0w57,50),
(0w65,0w90,50),
(0w95,0w95,50),
(0w97,0w122,50)], [0, 2]), ([], [33]), ([], [32]), ([], [37]), ([(0w101,0w101,122),
(0w114,0w114,123)], []), ([(0w110,0w110,118)], []), ([(0w101,0w101,113)], []), ([(0w109,0w109,108)], []), ([(0w101,0w101,101)], []), ([(0w97,0w97,91),
(0w111,0w111,92)], []), ([(0w101,0w101,85)], []), ([(0w116,0w116,81)], []), ([(0w111,0w111,74),
(0w114,0w114,75)], []), ([(0w97,0w97,70)], []), ([(0w104,0w104,66)], []), ([(0w101,0w101,67)], []), ([(0w114,0w114,68)], []), ([(0w101,0w101,69)], []), ([], [13]), ([(0w108,0w108,71)], []), ([(0w117,0w117,72)], []), ([(0w101,0w101,73)], []), ([], [6]), ([(0w107,0w107,77)], []), ([(0w121,0w121,76)], []), ([], [12]), ([(0w101,0w101,78)], []), ([(0w110,0w110,79)], []), ([(0w115,0w115,80)], [3]), ([], [3]), ([(0w97,0w97,82)], []), ([(0w114,0w114,83)], []), ([(0w116,0w116,84)], []), ([], [10]), ([(0w102,0w102,86)], []), ([(0w99,0w99,87)], []), ([(0w101,0w101,88)], []), ([(0w108,0w108,89)], []), ([(0w108,0w108,90)], []), ([], [15]), ([(0w109,0w109,99)], []), ([(0w110,0w110,93)], []), ([(0w116,0w116,94)], []), ([(0w101,0w101,95)], []), ([(0w114,0w114,96)], []), ([(0w109,0w109,97)], []), ([(0w115,0w115,98)], [7]), ([], [7]), ([(0w101,0w101,100)], []), ([], [9]), ([(0w121,0w121,102)], []), ([(0w119,0w119,103)], []), ([(0w111,0w111,104)], []), ([(0w114,0w114,105)], []), ([(0w100,0w100,106)], []), ([(0w115,0w115,107)], [5]), ([], [5]), ([(0w112,0w112,109)], []), ([(0w111,0w111,110)], []), ([(0w114,0w114,111)], []), ([(0w116,0w116,112)], []), ([], [8]), ([(0w97,0w97,114)], []), ([(0w100,0w100,115)], []), ([(0w101,0w101,116)], []), ([(0w114,0w114,117)], []), ([], [16]), ([(0w116,0w116,119)], []), ([(0w114,0w114,120)], []), ([(0w121,0w121,121)], []), ([], [11]), ([(0w102,0w102,130)], []), ([(0w111,0w111,124)], []), ([(0w112,0w112,125)], []), ([(0w112,0w112,126)], []), ([(0w105,0w105,127)], []), ([(0w110,0w110,128)], []), ([(0w103,0w103,129)], []), ([], [14]), ([(0w115,0w115,131)], []), ([], [4]), ([(0w9,0w12,132),
(0w32,0w32,132),
(0w13,0w13,133)], [1]), ([(0w9,0w12,132),
(0w32,0w32,132),
(0w13,0w13,133)], [1]), ([], [54, 72]), ([(0w10,0w10,161)], [54, 72]), ([], [70, 72]), ([(0w39,0w39,160),
(0w48,0w57,160),
(0w65,0w90,160),
(0w95,0w95,160),
(0w97,0w122,160)], [57, 72]), ([(0w42,0w42,159)], [65, 72]), ([], [66, 72]), ([], [61, 72]), ([], [63, 72]), ([(0w62,0w62,158)], [72]), ([], [62, 72]), ([], [64, 72]), ([], [71, 72]), ([(0w39,0w39,152),
(0w48,0w57,152),
(0w65,0w90,152),
(0w95,0w95,152),
(0w97,0w122,152),
(0w46,0w46,153)], [56, 72]), ([(0w39,0w39,152),
(0w48,0w57,152),
(0w65,0w90,152),
(0w95,0w95,152),
(0w97,0w104,152),
(0w106,0w122,152),
(0w46,0w46,153),
(0w105,0w105,155)], [56, 72]), ([(0w39,0w39,152),
(0w48,0w57,152),
(0w65,0w90,152),
(0w95,0w95,152),
(0w97,0w101,152),
(0w103,0w122,152),
(0w46,0w46,153),
(0w102,0w102,154)], [56, 72]), ([], [67, 72]), ([], [60, 72]), ([], [68, 72]), ([(0w39,0w39,152),
(0w48,0w57,152),
(0w65,0w90,152),
(0w95,0w95,152),
(0w97,0w122,152),
(0w46,0w46,153)], [56]), ([], [58]), ([(0w39,0w39,152),
(0w48,0w57,152),
(0w65,0w90,152),
(0w95,0w95,152),
(0w97,0w122,152),
(0w46,0w46,153)], [55, 56]), ([(0w39,0w39,152),
(0w48,0w57,152),
(0w65,0w90,152),
(0w95,0w95,152),
(0w97,0w102,152),
(0w104,0w122,152),
(0w46,0w46,153),
(0w103,0w103,156)], [56]), ([(0w39,0w39,152),
(0w48,0w57,152),
(0w65,0w90,152),
(0w95,0w95,152),
(0w97,0w104,152),
(0w106,0w122,152),
(0w46,0w46,153),
(0w105,0w105,157)], [56]), ([(0w39,0w39,152),
(0w48,0w57,152),
(0w65,0w90,152),
(0w95,0w95,152),
(0w97,0w115,152),
(0w117,0w122,152),
(0w46,0w46,153),
(0w116,0w116,157)], [56, 59]), ([], [69]), ([], [36]), ([(0w39,0w39,160),
(0w48,0w57,160),
(0w65,0w90,160),
(0w95,0w95,160),
(0w97,0w122,160)], [57]), ([], [54]), ([], [34, 72]), ([(0w42,0w42,165)], [26, 72]), ([], [27, 72]), ([], [35])]
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
	  fun yygetc strm = (case ULexBuffer.getu strm
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
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
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
       YYBEGIN PRECODE; Tok.KW_defs)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_keywords)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.KW_value)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_nonterms)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_import)
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_name)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_start)
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_entry)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_try)
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.KW_where)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_dropping)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CONSTR; Tok.KW_refcell)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.KW_header)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BAR)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.AT)
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.DOLLAR)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.PLUS)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.STAR)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.QUERY)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COLON)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SEMI)
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LP)
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RP)
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LSB)
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RSB)
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SLASH)
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.EQ)
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.ARROW)
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.DARROW)
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN INITIAL);
		    Tok.STRING (getText())
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue()
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN CONSTR);
	    continue()
      end
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN PRECODE);
	    continue()
      end
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN CODE);
	    continue()
      end
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
       comLvl := !comLvl+1; continue())
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
       comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue())
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;  continue())
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
       pcount := 1; YYBEGIN CODE; clrText(); continue())
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN PRECODE);
		    Tok.STRING (getText())
      end
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext;  (* NOTE: the initial "(" is consumed in the PRECODE state *)
		    inc pcount; continue()
      end
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         dec pcount; 
		    if !pcount = 0
		      then (YYBEGIN INITIAL; Tok.CODE (getText()))
		      else (addText yytext; continue())
      end
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    continue()
      end
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; Tok.BOGUS
      end
fun yyAction49 (strm, lastMatch : yymatch) = let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; err (!yylineno, !yycolno, "unclosed string");
 	            Tok.BOGUS
      end
fun yyAction50 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction51 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction52 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText(String.toString yytext); continue()
      end
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;  continue())
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.OF)
fun yyAction56 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction57 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.TYVAR yytext
      end
fun yyAction58 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.IDDOT yytext
      end
fun yyAction59 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.INT yytext
      end
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BAR)
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.STAR)
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COLON)
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction64 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.SEMI)
fun yyAction65 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LP)
fun yyAction66 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RP)
fun yyAction67 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LCB)
fun yyAction68 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RCB)
fun yyAction69 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.ARROW)
fun yyAction70 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN CONSTR);
		    Tok.STRING (getText())
      end
fun yyAction71 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.EQ)
fun yyAction72 (strm, lastMatch : yymatch) = let
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
  yyAction65, yyAction66, yyAction67, yyAction68, yyAction69, yyAction70,
  yyAction71, yyAction72])
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
        | PRECODE => yygo yyactTable (3, !(yystrm), yyNO_MATCH)
        | CONSTR => yygo yyactTable (4, !(yystrm), yyNO_MATCH)
        | INITIAL => yygo yyactTable (5, !(yystrm), yyNO_MATCH)
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
