structure MLULexLex  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1
	      }

	fun getc (Stream {strm, pos, id, lineNo}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0)
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof (Stream {strm, ...}) = TSIO.endOfStream strm

      end

    datatype 'a yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * 'a action * 'a yymatch
    withtype 'a action = yyInput.stream * 'a yymatch -> 'a

    datatype yystart_state = 
COM | CODE | STRING | CHARSET | CHARCLASS | INITIAL | DIRECTIVE
    structure UserDeclarations = 
      struct

(* ml-ulex-bootstrap.lex
 *
 * COPYRIGHT (c) 2006 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * (With some code borrowed from ml-yacc)
 *)

val comLvl : int ref = ref 0		(* nesting depth of comments *)
val comStart : int ref = ref 0		(* start line of current comment *)

fun eof () = (
      if (!comLvl > 0)
        then print("unclosed comment starting at line " ^ Int.toString(!comStart) ^ "\n")
        else ();
      Tok.EOF)

val text : string list ref = ref []
fun addText s = (text := s::(!text))
fun clrText () = (text := [])
fun getText () = concat (rev (!text))

val pcount = ref 0
fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

fun chomp s = String.substring (s, 1, String.size s - 2)



      end

    local
    fun mk yyins = let
        (* current start state *)
          val yyss = ref INITIAL
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yyins
	(* get one char of input *)
	  val yygetc = yyInput.getc 
	(* create yytext *)
	  fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
          open UserDeclarations
          fun lex 
(yyarg as ()) = let
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    fun continue() = 
let
fun yyAction0 (strm, lastMatch) = (yystrm := strm; (continue()))
fun yyAction1 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN CODE; clrText(); Tok.KW_defs))
fun yyAction2 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN DIRECTIVE; Tok.KW_name))
fun yyAction3 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN DIRECTIVE; Tok.KW_states))
fun yyAction4 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN DIRECTIVE; Tok.KW_let))
fun yyAction5 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN CHARSET; Tok.KW_charset))
fun yyAction6 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.ID yytext)
      end
fun yyAction7 (strm, lastMatch) = (yystrm := strm; (Tok.COMMA))
fun yyAction8 (strm, lastMatch) = (yystrm := strm; (YYBEGIN INITIAL; Tok.SEMI))
fun yyAction9 (strm, lastMatch) = (yystrm := strm; (YYBEGIN INITIAL; Tok.GT))
fun yyAction10 (strm, lastMatch) = (yystrm := strm; (YYBEGIN INITIAL; Tok.EQ))
fun yyAction11 (strm, lastMatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm; (YYBEGIN INITIAL; REJECT())
      end
fun yyAction12 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.UNICODE))
fun yyAction13 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.ASCII7))
fun yyAction14 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.ASCII8))
fun yyAction15 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.SEMI))
fun yyAction16 (strm, lastMatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm; (YYBEGIN INITIAL; REJECT())
      end
fun yyAction17 (strm, lastMatch) = (yystrm := strm; (Tok.BAR))
fun yyAction18 (strm, lastMatch) = (yystrm := strm; (Tok.DOT))
fun yyAction19 (strm, lastMatch) = (yystrm := strm; (Tok.DOLLAR))
fun yyAction20 (strm, lastMatch) = (yystrm := strm; (Tok.PLUS))
fun yyAction21 (strm, lastMatch) = (yystrm := strm; (Tok.STAR))
fun yyAction22 (strm, lastMatch) = (yystrm := strm; (Tok.QUERY))
fun yyAction23 (strm, lastMatch) = (yystrm := strm; (Tok.SEMI))
fun yyAction24 (strm, lastMatch) = (yystrm := strm; (Tok.LP))
fun yyAction25 (strm, lastMatch) = (yystrm := strm; (Tok.RP))
fun yyAction26 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN CHARCLASS; Tok.LSB))
fun yyAction27 (strm, lastMatch) = (yystrm := strm; (Tok.RSB))
fun yyAction28 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.ID (chomp yytext))
      end
fun yyAction29 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN DIRECTIVE; Tok.LT))
fun yyAction30 (strm, lastMatch) = (yystrm := strm; (Tok.GT))
fun yyAction31 (strm, lastMatch) = (yystrm := strm; (Tok.COMMA))
fun yyAction32 (strm, lastMatch) = (yystrm := strm; (Tok.SLASH))
fun yyAction33 (strm, lastMatch) = (yystrm := strm; (Tok.EQ))
fun yyAction34 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN CODE; clrText(); Tok.DARROW))
fun yyAction35 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN STRING; clrText(); 
		    ignore(continue() before YYBEGIN INITIAL);
		    (Tok.STRING o valOf o String.fromString o getText)()))
fun yyAction36 (strm, lastMatch) = (yystrm := strm; (Tok.CARAT))
fun yyAction37 (strm, lastMatch) = (yystrm := strm; (Tok.DASH))
fun yyAction38 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val c = Char.fromString yytext
            in case c
                of SOME c' => Tok.CHAR c'
		 | NONE => Tok.CHAR (String.sub (yytext, 1))
            end)
      end
fun yyAction39 (strm, lastMatch) = (yystrm := strm; (YYBEGIN INITIAL; Tok.RSB))
fun yyAction40 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.CHAR (String.sub (yytext, 0)))
      end
fun yyAction41 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm;
        (comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue())
      end
fun yyAction42 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    addText yytext;
	    ignore(continue() before YYBEGIN CODE);
	    continue())
      end
fun yyAction43 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; comLvl := !comLvl+1; continue())
      end
fun yyAction44 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addText yytext; comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue())
      end
fun yyAction45 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction46 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if !pcount = 0 then () else addText yytext;
		    inc pcount; continue())
      end
fun yyAction47 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL; Tok.CODE (getText()))
		    else (addText yytext; continue()))
      end
fun yyAction48 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    addText "\""; continue())
      end
fun yyAction49 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction50 (strm, lastMatch) = (yystrm := strm; (Tok.BOGUS))
fun yyAction51 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addText yytext; print ("unclosed string");
 	            Tok.BOGUS)
      end
fun yyAction52 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction53 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction54 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction55 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction56 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.CHAR (String.sub (yytext, 0)))
      end
fun yyAction57 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print (concat[Int.toString (!yylineno), ": illegal character '", 
			String.toCString yytext, "'\n"]);
	    continue())
      end
fun yyQ119 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ120 (strm, lastMatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ121 (strm, lastMatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ122 (strm, lastMatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ123 (strm, lastMatch) = yyAction11(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
fun yyQ127 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ127(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ127(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ127(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ127(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ124 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ127(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ127(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ127(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ127(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
and yyQ36 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ125 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ126 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyQ120(strm', lastMatch)
            else if inp < #";"
              then if inp = #"\^N"
                  then yyQ123(strm', lastMatch)
                else if inp < #"\^N"
                  then if inp = #"\n"
                      then yyQ36(strm', lastMatch)
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ126(strm', lastMatch)
                          else yyQ123(strm', lastMatch)
                    else if inp = #"\r"
                      then yyQ125(strm', lastMatch)
                      else yyQ126(strm', lastMatch)
                else if inp = #"!"
                  then yyQ123(strm', lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ126(strm', lastMatch)
                      else yyQ123(strm', lastMatch)
                else if inp = #","
                  then yyQ119(strm', lastMatch)
                  else yyQ123(strm', lastMatch)
            else if inp = #"A"
              then yyQ124(strm', lastMatch)
            else if inp < #"A"
              then if inp = #">"
                  then yyQ121(strm', lastMatch)
                else if inp < #">"
                  then if inp = #"<"
                      then yyQ123(strm', lastMatch)
                      else yyQ122(strm', lastMatch)
                  else yyQ123(strm', lastMatch)
            else if inp = #"a"
              then yyQ124(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ124(strm', lastMatch)
                  else yyQ123(strm', lastMatch)
            else if inp = #"{"
              then yyQ123(strm', lastMatch)
            else if inp < #"{"
              then yyQ124(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ123(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ68 (strm, lastMatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ69 (strm, lastMatch) = yyAction18(strm, yyNO_MATCH)
fun yyQ70 (strm, lastMatch) = yyAction19(strm, yyNO_MATCH)
fun yyQ71 (strm, lastMatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ72 (strm, lastMatch) = yyAction21(strm, yyNO_MATCH)
fun yyQ73 (strm, lastMatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ74 (strm, lastMatch) = yyAction23(strm, yyNO_MATCH)
fun yyQ118 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ75 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ118(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch) = yyAction25(strm, yyNO_MATCH)
fun yyQ77 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ78 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ79 (strm, lastMatch) = yyAction29(strm, yyNO_MATCH)
fun yyQ80 (strm, lastMatch) = yyAction30(strm, yyNO_MATCH)
fun yyQ81 (strm, lastMatch) = yyAction31(strm, yyNO_MATCH)
fun yyQ82 (strm, lastMatch) = yyAction32(strm, yyNO_MATCH)
fun yyQ117 (strm, lastMatch) = yyAction34(strm, yyNO_MATCH)
fun yyQ83 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ117(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch) = yyAction35(strm, yyNO_MATCH)
fun yyQ85 (strm, lastMatch) = yyAction56(strm, yyNO_MATCH)
fun yyQ65 (strm, lastMatch) = yyAction38(strm, yyNO_MATCH)
fun yyQ67 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ65(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ65(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ66 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ67(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ67(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ86 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction56(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction56(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction56(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ65(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = #"a"
              then yyQ65(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"\\"
                  then yyQ65(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ65(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ115 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ115(strm', lastMatch)
            else if inp < #"_"
              then if inp = #":"
                  then yystuck(lastMatch)
                else if inp < #":"
                  then if inp <= #"/"
                      then yystuck(lastMatch)
                      else yyQ115(strm', lastMatch)
                else if inp = #"A"
                  then yyQ115(strm', lastMatch)
                else if inp < #"A"
                  then yystuck(lastMatch)
                else if inp <= #"Z"
                  then yyQ115(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"{"
              then yystuck(lastMatch)
            else if inp < #"{"
              then if inp = #"`"
                  then yystuck(lastMatch)
                  else yyQ115(strm', lastMatch)
            else if inp = #"}"
              then yyQ116(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ87 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction56(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ115(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = #"a"
              then yyQ115(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #"a"
              then yyAction56(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ115(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ114 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ113 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ114(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ112 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ113(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ111 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ112(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ110 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ111(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ109 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ110(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ91 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"h"
              then yyQ109(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ108 (strm, lastMatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ107 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ108(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ92 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ107(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ106 (strm, lastMatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ105 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ106(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ104 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ103 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ104(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ102 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ103(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ93 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ102(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ101 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ100 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ101(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ99 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"m"
              then yyQ100(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ94 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ99(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ98 (strm, lastMatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ97 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ98(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ96 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"f"
              then yyQ97(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ95 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ96(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ88 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"m"
              then yyAction56(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"d"
                  then yyQ95(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp < #"d"
                  then if inp = #"c"
                      then yyQ91(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                      else yyAction56(strm, yyNO_MATCH)
                else if inp = #"l"
                  then yyQ92(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ93(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"n"
                  then yyQ94(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ85(strm', lastMatch)
            else if inp < #"-"
              then if inp = #"#"
                  then yyQ85(strm', lastMatch)
                else if inp < #"#"
                  then if inp = #"\r"
                      then yyQ89(strm', lastMatch)
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ36(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ90(strm', lastMatch)
                              else yyQ85(strm', lastMatch)
                          else yyQ90(strm', lastMatch)
                    else if inp = #"!"
                      then yyQ85(strm', lastMatch)
                    else if inp < #"!"
                      then if inp = #" "
                          then yyQ90(strm', lastMatch)
                          else yyQ85(strm', lastMatch)
                      else yyQ84(strm', lastMatch)
                else if inp = #")"
                  then yyQ76(strm', lastMatch)
                else if inp < #")"
                  then if inp = #"&"
                      then yyQ85(strm', lastMatch)
                    else if inp < #"&"
                      then if inp = #"$"
                          then yyQ70(strm', lastMatch)
                          else yyQ88(strm', lastMatch)
                    else if inp = #"("
                      then yyQ75(strm', lastMatch)
                      else yyQ85(strm', lastMatch)
                else if inp = #"+"
                  then yyQ71(strm', lastMatch)
                else if inp = #"*"
                  then yyQ72(strm', lastMatch)
                  else yyQ81(strm', lastMatch)
            else if inp = #"@"
              then yyQ85(strm', lastMatch)
            else if inp < #"@"
              then if inp = #"<"
                  then yyQ79(strm', lastMatch)
                else if inp < #"<"
                  then if inp = #"0"
                      then yyQ85(strm', lastMatch)
                    else if inp < #"0"
                      then if inp = #"."
                          then yyQ69(strm', lastMatch)
                          else yyQ82(strm', lastMatch)
                    else if inp = #";"
                      then yyQ74(strm', lastMatch)
                      else yyQ85(strm', lastMatch)
                else if inp = #">"
                  then yyQ80(strm', lastMatch)
                else if inp = #"="
                  then yyQ83(strm', lastMatch)
                  else yyQ73(strm', lastMatch)
            else if inp = #"^"
              then yyQ85(strm', lastMatch)
            else if inp < #"^"
              then if inp = #"\\"
                  then yyQ86(strm', lastMatch)
                else if inp < #"\\"
                  then if inp = #"["
                      then yyQ77(strm', lastMatch)
                      else yyQ85(strm', lastMatch)
                  else yyQ78(strm', lastMatch)
            else if inp = #"|"
              then yyQ68(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"{"
                  then yyQ87(strm', lastMatch)
                  else yyQ85(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ85(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ58 (strm, lastMatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ59 (strm, lastMatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ60 (strm, lastMatch) = yyAction39(strm, yyNO_MATCH)
fun yyQ61 (strm, lastMatch) = yyAction40(strm, yyNO_MATCH)
fun yyQ62 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction40(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction40(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction40(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction40(strm, yyNO_MATCH)
                  else yyQ65(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp = #"a"
              then yyQ65(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"\\"
                  then yyQ65(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                  else yyAction40(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ65(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyQ61(strm', lastMatch)
            else if inp < #"!"
              then if inp = #"\v"
                  then yyQ64(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\t"
                      then yyQ64(strm', lastMatch)
                    else if inp = #"\n"
                      then yyQ36(strm', lastMatch)
                      else yyQ61(strm', lastMatch)
                else if inp = #"\^N"
                  then yyQ61(strm', lastMatch)
                else if inp < #"\^N"
                  then if inp = #"\r"
                      then yyQ63(strm', lastMatch)
                      else yyQ64(strm', lastMatch)
                else if inp = #" "
                  then yyQ64(strm', lastMatch)
                  else yyQ61(strm', lastMatch)
            else if inp = #"]"
              then yyQ60(strm', lastMatch)
            else if inp < #"]"
              then if inp = #"."
                  then yyQ61(strm', lastMatch)
                else if inp < #"."
                  then if inp = #"-"
                      then yyQ59(strm', lastMatch)
                      else yyQ61(strm', lastMatch)
                else if inp = #"\\"
                  then yyQ62(strm', lastMatch)
                  else yyQ61(strm', lastMatch)
            else if inp = #"_"
              then yyQ61(strm', lastMatch)
            else if inp < #"_"
              then yyQ58(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ61(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ28 (strm, lastMatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ29 (strm, lastMatch) = yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
fun yyQ53 (strm, lastMatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ54 (strm, lastMatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ52 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"8"
              then yyQ54(strm', lastMatch)
            else if inp < #"8"
              then if inp = #"7"
                  then yyQ53(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ57 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"I"
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ56 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"I"
              then yyQ57(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ55 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"C"
              then yyQ56(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ30 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"S"
              then yyQ55(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction57, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
      (* end case *))
fun yyQ51 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ50 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ51(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ50(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ31 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ49(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction57, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
      (* end case *))
fun yyQ43 (strm, lastMatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ48 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ43(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ47 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"D"
              then yyQ48(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ46 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"O"
              then yyQ47(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ45 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"C"
              then yyQ46(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"I"
              then yyQ45(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ32 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"N"
              then yyQ44(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction57, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
      (* end case *))
fun yyQ42 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ43(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ41 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"d"
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ40 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"o"
              then yyQ41(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ39 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ40(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ38 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"n"
              then yyQ38(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction57, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
      (* end case *))
fun yyQ34 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"<"
              then yyQ29(strm', lastMatch)
            else if inp < #"<"
              then if inp = #"\r"
                  then yyQ34(strm', lastMatch)
                else if inp < #"\r"
                  then if inp = #"\n"
                      then yyQ36(strm', lastMatch)
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ35(strm', lastMatch)
                          else yyQ29(strm', lastMatch)
                      else yyQ35(strm', lastMatch)
                else if inp = #"!"
                  then yyQ29(strm', lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ35(strm', lastMatch)
                      else yyQ29(strm', lastMatch)
                else if inp = #";"
                  then yyQ28(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"a"
              then yyQ31(strm', lastMatch)
            else if inp < #"a"
              then if inp = #"B"
                  then yyQ29(strm', lastMatch)
                else if inp < #"B"
                  then if inp = #"A"
                      then yyQ30(strm', lastMatch)
                      else yyQ29(strm', lastMatch)
                else if inp = #"U"
                  then yyQ32(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"v"
              then yyQ29(strm', lastMatch)
            else if inp < #"v"
              then if inp = #"u"
                  then yyQ33(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ29(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ20 (strm, lastMatch) = yyAction50(strm, yyNO_MATCH)
fun yyQ21 (strm, lastMatch) = yyAction51(strm, yyNO_MATCH)
fun yyQ26 (strm, lastMatch) = yyAction53(strm, yyNO_MATCH)
fun yyQ27 (strm, lastMatch) = yyAction55(strm, yyNO_MATCH)
fun yyQ22 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyAction52(strm, yyNO_MATCH)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ27(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                  else yyAction52(strm, yyNO_MATCH)
            else if inp = #"\\"
              then yyQ26(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyAction54(strm, yyNO_MATCH)
            else if inp < #"\""
              then if inp = #"\v"
                  then yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyAction54(strm, yyNO_MATCH)
                      else yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                else if inp = #"\r"
                  then yyAction54(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
            else if inp = #"]"
              then yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
            else if inp < #"]"
              then if inp = #"\\"
                  then yyAction54(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyAction54(strm, yyNO_MATCH)
            else if inp < #"\""
              then if inp = #"\v"
                  then yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyAction54(strm, yyNO_MATCH)
                      else yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                else if inp = #"\r"
                  then yyAction54(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
            else if inp = #"]"
              then yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
            else if inp < #"]"
              then if inp = #"\\"
                  then yyAction54(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
            else if inp <= #"\127"
              then yyQ25(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ21(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
              else yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ20(strm', lastMatch)
            else if inp < #"\""
              then if inp = #"\v"
                  then yyQ23(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ21(strm', lastMatch)
                      else yyQ23(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ24(strm', lastMatch)
                  else yyQ23(strm', lastMatch)
            else if inp = #"]"
              then yyQ23(strm', lastMatch)
            else if inp < #"]"
              then if inp = #"\\"
                  then yyQ22(strm', lastMatch)
                  else yyQ23(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ23(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ14 (strm, lastMatch) = yyAction47(strm, yyNO_MATCH)
fun yyQ15 (strm, lastMatch) = yyAction48(strm, yyNO_MATCH)
fun yyQ17 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"("
              then yyAction49(strm, yyNO_MATCH)
            else if inp < #"("
              then if inp = #"\""
                  then yyAction49(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp = #"*"
              then yyQ17(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < #"*"
              then yyAction49(strm, yyNO_MATCH)
            else if inp <= #"\127"
              then yyQ17(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"("
              then yyAction49(strm, yyNO_MATCH)
            else if inp < #"("
              then if inp = #"\""
                  then yyAction49(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp = #"*"
              then yyQ17(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < #"*"
              then yyAction49(strm, yyNO_MATCH)
            else if inp <= #"\127"
              then yyQ17(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch) = yyAction42(strm, yyNO_MATCH)
fun yyQ18 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ19(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ16(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\v"
                  then yyQ16(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ17(strm', lastMatch)
                      else yyQ16(strm', lastMatch)
                else if inp = #"\""
                  then yyQ15(strm', lastMatch)
                  else yyQ16(strm', lastMatch)
            else if inp = #")"
              then yyQ14(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ18(strm', lastMatch)
                  else yyQ16(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ16(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ7 (strm, lastMatch) = yyAction45(strm, yyNO_MATCH)
fun yyQ8 (strm, lastMatch) = yyAction45(strm, yyNO_MATCH)
fun yyQ9 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ8(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ13(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ11 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ12(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"("
              then yyQ11(strm', lastMatch)
            else if inp < #"("
              then if inp = #"\v"
                  then yyQ7(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ8(strm', lastMatch)
                      else yyQ7(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ9(strm', lastMatch)
                  else yyQ7(strm', lastMatch)
            else if inp = #"+"
              then yyQ7(strm', lastMatch)
            else if inp < #"+"
              then if inp = #")"
                  then yyQ7(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp <= #"\127"
              then yyQ7(strm', lastMatch)
            else if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COM => yyQ0(!(yystrm), yyNO_MATCH)
    | CODE => yyQ1(!(yystrm), yyNO_MATCH)
    | STRING => yyQ2(!(yystrm), yyNO_MATCH)
    | CHARSET => yyQ3(!(yystrm), yyNO_MATCH)
    | CHARCLASS => yyQ4(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ5(!(yystrm), yyNO_MATCH)
    | DIRECTIVE => yyQ6(!(yystrm), yyNO_MATCH)
  (* end case *))
end
	    in continue() end
          in 
            lex 
          end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
