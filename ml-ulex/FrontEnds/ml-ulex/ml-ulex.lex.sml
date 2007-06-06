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

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
#[
([(0w0,0w12,9),
(0w14,0w39,9),
(0w41,0w41,9),
(0w43,0w2147483647,9),
(0w13,0w13,10),
(0w40,0w40,11),
(0w42,0w42,12)], []), ([(0w0,0w33,16),
(0w35,0w39,16),
(0w42,0w2147483647,16),
(0w34,0w34,17),
(0w40,0w40,18),
(0w41,0w41,19)], []), ([(0w0,0w9,22),
(0w11,0w12,22),
(0w14,0w33,22),
(0w35,0w91,22),
(0w93,0w2147483647,22),
(0w10,0w10,23),
(0w13,0w13,24),
(0w34,0w34,25),
(0w92,0w92,26)], []), ([(0w0,0w8,31),
(0w14,0w31,31),
(0w33,0w58,31),
(0w60,0w64,31),
(0w66,0w84,31),
(0w86,0w96,31),
(0w98,0w116,31),
(0w118,0w2147483647,31),
(0w9,0w12,32),
(0w32,0w32,32),
(0w13,0w13,33),
(0w59,0w59,34),
(0w65,0w65,35),
(0w85,0w85,36),
(0w97,0w97,37),
(0w117,0w117,38)], []), ([(0w0,0w8,54),
(0w14,0w31,54),
(0w33,0w44,54),
(0w46,0w91,54),
(0w95,0w2147483647,54),
(0w9,0w9,55),
(0w11,0w12,55),
(0w32,0w32,55),
(0w10,0w10,56),
(0w13,0w13,57),
(0w45,0w45,58),
(0w92,0w92,59),
(0w93,0w93,60),
(0w94,0w94,61)], []), ([(0w0,0w43,77),
(0w45,0w47,77),
(0w58,0w64,77),
(0w91,0w96,77),
(0w123,0w124,77),
(0w126,0w2147483647,77),
(0w44,0w44,78),
(0w48,0w57,79),
(0w65,0w90,80),
(0w97,0w122,80),
(0w125,0w125,81)], []), ([(0w0,0w9,84),
(0w11,0w12,84),
(0w14,0w33,84),
(0w35,0w91,84),
(0w93,0w2147483647,84),
(0w10,0w10,85),
(0w13,0w13,86),
(0w34,0w34,87),
(0w92,0w92,88)], []), ([(0w0,0w8,90),
(0w14,0w31,90),
(0w33,0w33,90),
(0w35,0w35,90),
(0w39,0w39,90),
(0w45,0w45,90),
(0w48,0w58,90),
(0w64,0w90,90),
(0w95,0w122,90),
(0w127,0w2147483647,90),
(0w9,0w9,91),
(0w11,0w12,91),
(0w32,0w32,91),
(0w10,0w10,56),
(0w13,0w13,92),
(0w34,0w34,93),
(0w36,0w36,94),
(0w37,0w37,95),
(0w38,0w38,96),
(0w40,0w40,97),
(0w41,0w41,98),
(0w42,0w42,99),
(0w43,0w43,100),
(0w44,0w44,101),
(0w46,0w46,102),
(0w47,0w47,103),
(0w59,0w59,104),
(0w60,0w60,105),
(0w61,0w61,106),
(0w62,0w62,107),
(0w63,0w63,108),
(0w91,0w91,109),
(0w92,0w92,110),
(0w93,0w93,111),
(0w94,0w94,112),
(0w123,0w123,113),
(0w124,0w124,114),
(0w125,0w125,77),
(0w126,0w126,115)], []), ([(0w0,0w8,151),
(0w14,0w31,151),
(0w33,0w43,151),
(0w45,0w58,151),
(0w60,0w60,151),
(0w63,0w64,151),
(0w91,0w96,151),
(0w123,0w2147483647,151),
(0w9,0w12,152),
(0w32,0w32,152),
(0w13,0w13,153),
(0w44,0w44,154),
(0w59,0w59,155),
(0w61,0w61,156),
(0w62,0w62,157),
(0w65,0w90,158),
(0w97,0w122,158)], []), ([], [55, 70]), ([(0w10,0w10,15)], [55, 70]), ([(0w42,0w42,14)], [55, 70]), ([(0w41,0w41,13)], [55, 70]), ([], [54]), ([], [53]), ([], [55]), ([(0w0,0w33,21),
(0w35,0w39,21),
(0w42,0w2147483647,21)], [59, 70]), ([], [58, 70]), ([(0w42,0w42,20)], [56, 70]), ([], [57, 70]), ([], [52]), ([(0w0,0w33,21),
(0w35,0w39,21),
(0w42,0w2147483647,21)], [59]), ([(0w0,0w9,30),
(0w11,0w12,30),
(0w14,0w33,30),
(0w35,0w91,30),
(0w93,0w2147483647,30)], [65, 70]), ([], [61, 70]), ([(0w10,0w10,29)], [61, 70]), ([], [60, 70]), ([(0w34,0w34,27),
(0w92,0w92,28)], [62, 70]), ([], [64]), ([], [63]), ([], [61]), ([(0w0,0w9,30),
(0w11,0w12,30),
(0w14,0w33,30),
(0w35,0w91,30),
(0w93,0w2147483647,30)], [65]), ([], [18, 70]), ([(0w9,0w12,52),
(0w32,0w32,52),
(0w13,0w13,53)], [0, 18, 70]), ([(0w9,0w12,52),
(0w32,0w32,52),
(0w13,0w13,53)], [0, 18, 70]), ([], [17, 18, 70]), ([(0w83,0w83,49)], [18, 70]), ([(0w84,0w84,48)], [18, 70]), ([(0w115,0w115,42)], [18, 70]), ([(0w116,0w116,39)], [18, 70]), ([(0w102,0w102,40)], []), ([(0w56,0w56,41)], []), ([], [14]), ([(0w99,0w99,43)], []), ([(0w105,0w105,44)], []), ([(0w105,0w105,45)], []), ([(0w55,0w55,46),
(0w56,0w56,47)], []), ([], [15]), ([], [16]), ([(0w70,0w70,40)], []), ([(0w67,0w67,50)], []), ([(0w73,0w73,51)], []), ([(0w73,0w73,45)], []), ([(0w9,0w12,52),
(0w32,0w32,52),
(0w13,0w13,53)], [0]), ([(0w9,0w12,52),
(0w32,0w32,52),
(0w13,0w13,53)], [0]), ([], [50, 70]), ([(0w9,0w12,52),
(0w32,0w32,52),
(0w13,0w13,53)], [0, 50, 70]), ([(0w9,0w12,52),
(0w32,0w32,52),
(0w13,0w13,53)], [0, 70]), ([(0w9,0w12,52),
(0w32,0w32,52),
(0w13,0w13,53)], [0, 50, 70]), ([], [45, 50, 70]), ([(0w0,0w33,62),
(0w35,0w47,62),
(0w58,0w64,62),
(0w91,0w91,62),
(0w93,0w96,62),
(0w123,0w2147483647,62),
(0w34,0w34,63),
(0w92,0w92,63),
(0w48,0w57,64),
(0w65,0w84,65),
(0w86,0w90,65),
(0w97,0w116,65),
(0w118,0w122,65),
(0w85,0w85,66),
(0w117,0w117,67)], [70]), ([], [49, 50, 70]), ([], [44, 50, 70]), ([], [48]), ([], [46, 48]), ([(0w48,0w57,76)], [48]), ([], [46]), ([(0w48,0w57,72),
(0w65,0w90,72),
(0w97,0w122,72)], [46]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w97,0w122,68)], [46]), ([(0w48,0w57,69),
(0w65,0w90,69),
(0w97,0w122,69)], []), ([(0w48,0w57,70),
(0w65,0w90,70),
(0w97,0w122,70)], []), ([(0w48,0w57,71),
(0w65,0w90,71),
(0w97,0w122,71)], []), ([], [47]), ([(0w48,0w57,73),
(0w65,0w90,73),
(0w97,0w122,73)], []), ([(0w48,0w57,74),
(0w65,0w90,74),
(0w97,0w122,74)], []), ([(0w48,0w57,75),
(0w65,0w90,75),
(0w97,0w122,75)], []), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w97,0w122,68)], []), ([(0w48,0w57,65)], []), ([], [70]), ([], [36, 70]), ([(0w48,0w57,83)], [35, 70]), ([(0w48,0w57,82),
(0w65,0w90,82),
(0w95,0w95,82),
(0w97,0w122,82)], [34, 70]), ([], [33, 70]), ([(0w48,0w57,82),
(0w65,0w90,82),
(0w95,0w95,82),
(0w97,0w122,82)], [34]), ([(0w48,0w57,83)], [35]), ([], [68, 70]), ([], [67, 68, 70]), ([(0w10,0w10,89)], [67, 68, 70]), ([], [66, 68, 70]), ([(0w0,0w33,62),
(0w35,0w47,62),
(0w58,0w64,62),
(0w91,0w91,62),
(0w93,0w96,62),
(0w123,0w2147483647,62),
(0w34,0w34,63),
(0w92,0w92,63),
(0w48,0w57,64),
(0w65,0w84,65),
(0w86,0w90,65),
(0w97,0w116,65),
(0w118,0w122,65),
(0w85,0w85,66),
(0w117,0w117,67)], [68, 70]), ([], [67]), ([], [69, 70]), ([(0w9,0w12,52),
(0w32,0w32,52),
(0w13,0w13,53)], [0, 69, 70]), ([(0w9,0w12,52),
(0w32,0w32,52),
(0w13,0w13,53)], [0, 69, 70]), ([], [43, 69, 70]), ([], [21, 69, 70]), ([(0w97,0w97,124),
(0w99,0w99,125),
(0w100,0w100,126),
(0w108,0w108,127),
(0w110,0w110,128),
(0w115,0w115,129)], [69, 70]), ([], [23, 69, 70]), ([(0w42,0w42,123)], [28, 69, 70]), ([], [29, 69, 70]), ([], [24, 69, 70]), ([], [22, 69, 70]), ([], [39, 69, 70]), ([], [20, 69, 70]), ([], [40, 69, 70]), ([], [27, 70]), ([(0w60,0w60,117)], [37, 69, 70]), ([(0w62,0w62,116)], [41, 69, 70]), ([], [38, 69, 70]), ([], [25, 69, 70]), ([], [30, 69, 70]), ([(0w0,0w33,62),
(0w35,0w47,62),
(0w58,0w64,62),
(0w91,0w91,62),
(0w93,0w96,62),
(0w123,0w2147483647,62),
(0w34,0w34,63),
(0w92,0w92,63),
(0w48,0w57,64),
(0w65,0w84,65),
(0w86,0w90,65),
(0w97,0w116,65),
(0w118,0w122,65),
(0w85,0w85,66),
(0w117,0w117,67)], [69, 70]), ([], [31, 69, 70]), ([], [44, 69, 70]), ([], [32, 70]), ([], [19, 69, 70]), ([], [26, 69, 70]), ([], [42]), ([(0w69,0w69,118)], []), ([(0w79,0w79,119)], []), ([(0w70,0w70,120)], []), ([(0w62,0w62,121)], []), ([(0w62,0w62,122)], []), ([], [7]), ([], [51]), ([(0w114,0w114,149)], []), ([(0w104,0w104,143)], []), ([(0w101,0w101,140)], []), ([(0w101,0w101,138)], []), ([(0w97,0w97,135)], []), ([(0w116,0w116,130)], []), ([(0w97,0w97,131)], []), ([(0w116,0w116,132)], []), ([(0w101,0w101,133)], []), ([(0w115,0w115,134)], []), ([], [4]), ([(0w109,0w109,136)], []), ([(0w101,0w101,137)], []), ([], [3]), ([(0w116,0w116,139)], []), ([], [5]), ([(0w102,0w102,141)], []), ([(0w115,0w115,142)], []), ([], [1]), ([(0w97,0w97,144)], []), ([(0w114,0w114,145)], []), ([(0w115,0w115,146)], []), ([(0w101,0w101,147)], []), ([(0w116,0w116,148)], []), ([], [6]), ([(0w103,0w103,150)], []), ([], [2]), ([], [13, 70]), ([(0w9,0w12,52),
(0w32,0w32,52),
(0w13,0w13,53)], [0, 13, 70]), ([(0w9,0w12,52),
(0w32,0w32,52),
(0w13,0w13,53)], [0, 13, 70]), ([], [9, 13, 70]), ([], [10, 13, 70]), ([], [11, 13, 70]), ([], [12, 13, 70]), ([(0w48,0w57,159),
(0w65,0w90,159),
(0w95,0w95,159),
(0w97,0w122,159)], [8, 13, 70]), ([(0w48,0w57,159),
(0w65,0w90,159),
(0w95,0w95,159),
(0w97,0w122,159)], [8])]

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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.KW_defs)
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.KW_arg)
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_name)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_states)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.KW_let)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CHARSET; Tok.KW_charset)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.EOFMARK)
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.SEMI)
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.EQ)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.GT)
fun yyAction13 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm;  YYBEGIN INITIAL; REJECT()
      end
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.UTF8)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.ASCII7)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.ASCII8)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.SEMI)
fun yyAction18 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm;  YYBEGIN INITIAL; REJECT()
      end
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BAR)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.DOT)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.DOLLAR)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.PLUS)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.AMP)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.STAR)
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.QUERY)
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.NEG)
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SEMI)
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LP)
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RP)
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CHARCLASS; Tok.LSB)
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RSB)
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CURLY; Tok.LCB)
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.RCB)
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.INT (valOf (Int.fromString yytext))
      end
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN DIRECTIVE; Tok.LT)
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.GT)
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SLASH)
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.EQ)
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.DARROW)
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN RESTRING; continue())
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.CARAT)
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.DASH)
fun yyAction46 (strm, lastMatch : yymatch) = let
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
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;  Tok.UCHAR (hexVal (Substring.triml 2 yysubstr))
      end
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.CHAR (String.sub (yytext, 1))
      end
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.RSB)
fun yyAction50 (strm, lastMatch : yymatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  mkUChar yyunicode
      end
fun yyAction51 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue()
      end
fun yyAction52 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    addText yytext;
	    ignore(continue() before YYBEGIN CODE);
	    continue()
      end
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; comLvl := !comLvl+1; continue()
      end
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue()
      end
fun yyAction55 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction56 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         if !pcount = 0 then () else addText yytext;
		    inc pcount; continue()
      end
fun yyAction57 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL; Tok.CODE (getText()))
		    else (addText yytext; continue())
      end
fun yyAction58 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    addText "\""; continue()
      end
fun yyAction59 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BOGUS)
fun yyAction61 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; print ("unclosed string\n");
 	            Tok.BOGUS
      end
fun yyAction62 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction63 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction64 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction65 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction66 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; continue())
fun yyAction67 (strm, lastMatch : yymatch) = (yystrm := strm;
       print ("unclosed string\n"); continue())
fun yyAction68 (strm, lastMatch : yymatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  mkUChar yyunicode
      end
fun yyAction69 (strm, lastMatch : yymatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  mkUChar yyunicode
      end
fun yyAction70 (strm, lastMatch : yymatch) = let
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
  yyAction65, yyAction66, yyAction67, yyAction68, yyAction69, yyAction70])
in
  if ULexBuffer.eof(!(yystrm))
    then let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        (case (!(yyss))
         of STRING =>
              ( 
	   print (Int.toString (!yylineno) ^ ": unclosed string\n");
	    Tok.EOF)
          | RESTRING =>
              ( 
	   print (Int.toString (!yylineno) ^ ": unclosed string\n");
	    Tok.EOF)
          | _ => ( Tok.EOF)
        (* end case *))
      end
    else (case (!(yyss))
       of COM => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
        | CODE => yygo yyactTable (1, !(yystrm), yyNO_MATCH)
        | STRING => yygo yyactTable (2, !(yystrm), yyNO_MATCH)
        | CHARSET => yygo yyactTable (3, !(yystrm), yyNO_MATCH)
        | CHARCLASS => yygo yyactTable (4, !(yystrm), yyNO_MATCH)
        | CURLY => yygo yyactTable (5, !(yystrm), yyNO_MATCH)
        | RESTRING => yygo yyactTable (6, !(yystrm), yyNO_MATCH)
        | INITIAL => yygo yyactTable (7, !(yystrm), yyNO_MATCH)
        | DIRECTIVE => yygo yyactTable (8, !(yystrm), yyNO_MATCH)
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
