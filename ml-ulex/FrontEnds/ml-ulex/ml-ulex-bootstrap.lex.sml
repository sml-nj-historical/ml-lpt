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
      (YYBEGIN INITIAL; Tok.UTF8))
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
fun yyAction18 (strm, lastMatch) = (yystrm := strm; (Tok.AMP))
fun yyAction19 (strm, lastMatch) = (yystrm := strm; (Tok.DOT))
fun yyAction20 (strm, lastMatch) = (yystrm := strm; (Tok.DOLLAR))
fun yyAction21 (strm, lastMatch) = (yystrm := strm; (Tok.PLUS))
fun yyAction22 (strm, lastMatch) = (yystrm := strm; (Tok.STAR))
fun yyAction23 (strm, lastMatch) = (yystrm := strm; (Tok.QUERY))
fun yyAction24 (strm, lastMatch) = (yystrm := strm; (Tok.SEMI))
fun yyAction25 (strm, lastMatch) = (yystrm := strm; (Tok.LP))
fun yyAction26 (strm, lastMatch) = (yystrm := strm; (Tok.RP))
fun yyAction27 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN CHARCLASS; Tok.LSB))
fun yyAction28 (strm, lastMatch) = (yystrm := strm; (Tok.RSB))
fun yyAction29 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.ID (chomp yytext))
      end
fun yyAction30 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN DIRECTIVE; Tok.LT))
fun yyAction31 (strm, lastMatch) = (yystrm := strm; (Tok.GT))
fun yyAction32 (strm, lastMatch) = (yystrm := strm; (Tok.COMMA))
fun yyAction33 (strm, lastMatch) = (yystrm := strm; (Tok.SLASH))
fun yyAction34 (strm, lastMatch) = (yystrm := strm; (Tok.EQ))
fun yyAction35 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN CODE; clrText(); Tok.DARROW))
fun yyAction36 (strm, lastMatch) = (yystrm := strm;
      (YYBEGIN STRING; clrText(); 
		    ignore(continue() before YYBEGIN INITIAL);
		    (Tok.STRING o valOf o String.fromString o getText)()))
fun yyAction37 (strm, lastMatch) = (yystrm := strm; (Tok.CARAT))
fun yyAction38 (strm, lastMatch) = (yystrm := strm; (Tok.DASH))
fun yyAction39 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val c = Char.fromString yytext
            in case c
                of SOME c' => Tok.CHAR c'
		 | NONE => Tok.CHAR (String.sub (yytext, 1))
            end)
      end
fun yyAction40 (strm, lastMatch) = (yystrm := strm; (YYBEGIN INITIAL; Tok.RSB))
fun yyAction41 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.CHAR (String.sub (yytext, 0)))
      end
fun yyAction42 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm;
        (comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue())
      end
fun yyAction43 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    addText yytext;
	    ignore(continue() before YYBEGIN CODE);
	    continue())
      end
fun yyAction44 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; comLvl := !comLvl+1; continue())
      end
fun yyAction45 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addText yytext; comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue())
      end
fun yyAction46 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction47 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if !pcount = 0 then () else addText yytext;
		    inc pcount; continue())
      end
fun yyAction48 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL; Tok.CODE (getText()))
		    else (addText yytext; continue()))
      end
fun yyAction49 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    addText "\""; continue())
      end
fun yyAction50 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction51 (strm, lastMatch) = (yystrm := strm; (Tok.BOGUS))
fun yyAction52 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addText yytext; print ("unclosed string");
 	            Tok.BOGUS)
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
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction57 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.CHAR (String.sub (yytext, 0)))
      end
fun yyAction58 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print (concat[Int.toString (!yylineno), ": illegal character '", 
			String.toCString yytext, "'\n"]);
	    continue())
      end
fun yyQ114 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ115 (strm, lastMatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ116 (strm, lastMatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ117 (strm, lastMatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ118 (strm, lastMatch) = yyAction11(strm, yyMATCH(strm, yyAction58, yyNO_MATCH))
fun yyQ122 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
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
fun yyQ120 (strm, lastMatch) = (case (yygetc(strm))
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
fun yyQ121 (strm, lastMatch) = (case (yygetc(strm))
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
            if inp = #"-"
              then yyQ118(strm', lastMatch)
            else if inp < #"-"
              then if inp = #"\r"
                  then yyQ120(strm', lastMatch)
                else if inp < #"\r"
                  then if inp = #"\n"
                      then yyQ36(strm', lastMatch)
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ121(strm', lastMatch)
                          else yyQ118(strm', lastMatch)
                      else yyQ121(strm', lastMatch)
                else if inp = #"!"
                  then yyQ118(strm', lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ121(strm', lastMatch)
                      else yyQ118(strm', lastMatch)
                else if inp = #","
                  then yyQ114(strm', lastMatch)
                  else yyQ118(strm', lastMatch)
            else if inp = #"?"
              then yyQ118(strm', lastMatch)
            else if inp < #"?"
              then if inp = #"<"
                  then yyQ118(strm', lastMatch)
                else if inp < #"<"
                  then if inp = #";"
                      then yyQ115(strm', lastMatch)
                      else yyQ118(strm', lastMatch)
                else if inp = #"="
                  then yyQ117(strm', lastMatch)
                  else yyQ116(strm', lastMatch)
            else if inp = #"["
              then yyQ118(strm', lastMatch)
            else if inp < #"["
              then if inp <= #"@"
                  then yyQ118(strm', lastMatch)
                  else yyQ119(strm', lastMatch)
            else if inp = #"a"
              then yyQ119(strm', lastMatch)
            else if inp < #"a"
              then yyQ118(strm', lastMatch)
            else if inp <= #"z"
              then yyQ119(strm', lastMatch)
              else yyQ118(strm', lastMatch)
      (* end case *))
fun yyQ61 (strm, lastMatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ62 (strm, lastMatch) = yyAction18(strm, yyNO_MATCH)
fun yyQ63 (strm, lastMatch) = yyAction19(strm, yyNO_MATCH)
fun yyQ64 (strm, lastMatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ65 (strm, lastMatch) = yyAction21(strm, yyNO_MATCH)
fun yyQ66 (strm, lastMatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ67 (strm, lastMatch) = yyAction23(strm, yyNO_MATCH)
fun yyQ68 (strm, lastMatch) = yyAction24(strm, yyNO_MATCH)
fun yyQ113 (strm, lastMatch) = yyAction42(strm, yyNO_MATCH)
fun yyQ69 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ113(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ71 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ72 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ73 (strm, lastMatch) = yyAction30(strm, yyNO_MATCH)
fun yyQ74 (strm, lastMatch) = yyAction31(strm, yyNO_MATCH)
fun yyQ75 (strm, lastMatch) = yyAction32(strm, yyNO_MATCH)
fun yyQ76 (strm, lastMatch) = yyAction33(strm, yyNO_MATCH)
fun yyQ112 (strm, lastMatch) = yyAction35(strm, yyNO_MATCH)
fun yyQ77 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ112(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ79 (strm, lastMatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ80 (strm, lastMatch) = yyAction57(strm, yyNO_MATCH)
fun yyQ58 (strm, lastMatch) = yyAction39(strm, yyNO_MATCH)
fun yyQ60 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ58(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ58(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ59 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ60(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ60(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ81 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction57(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction57(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction57(strm, yyNO_MATCH)
                      else yyQ59(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction57(strm, yyNO_MATCH)
                  else yyQ58(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp = #"a"
              then yyQ58(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"\\"
                  then yyQ58(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                  else yyAction57(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ58(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
              else yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch) = yyAction29(strm, yyNO_MATCH)
fun yyQ110 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ110(strm', lastMatch)
            else if inp < #"_"
              then if inp = #":"
                  then yystuck(lastMatch)
                else if inp < #":"
                  then if inp <= #"/"
                      then yystuck(lastMatch)
                      else yyQ110(strm', lastMatch)
                else if inp = #"A"
                  then yyQ110(strm', lastMatch)
                else if inp < #"A"
                  then yystuck(lastMatch)
                else if inp <= #"Z"
                  then yyQ110(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"{"
              then yystuck(lastMatch)
            else if inp < #"{"
              then if inp = #"`"
                  then yystuck(lastMatch)
                  else yyQ110(strm', lastMatch)
            else if inp = #"}"
              then yyQ111(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ82 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction57(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction57(strm, yyNO_MATCH)
                  else yyQ110(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp = #"a"
              then yyQ110(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp < #"a"
              then yyAction57(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ110(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
              else yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ108 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ109(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ107 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ108(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ106 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ107(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ105 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ106(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ104 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ86 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"h"
              then yyQ104(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ103 (strm, lastMatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ102 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ103(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ87 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ102(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ101 (strm, lastMatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ100 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ101(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ99 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ100(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ98 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ99(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ97 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ98(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ88 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ97(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ96 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ95 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ96(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ94 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"m"
              then yyQ95(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ89 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ94(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ93 (strm, lastMatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ92 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ93(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ91 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"f"
              then yyQ92(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ90 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ91(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ83 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"m"
              then yyAction57(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"d"
                  then yyQ90(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp < #"d"
                  then if inp = #"c"
                      then yyQ86(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                      else yyAction57(strm, yyNO_MATCH)
                else if inp = #"l"
                  then yyQ87(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                  else yyAction57(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ88(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"n"
                  then yyQ89(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                  else yyAction57(strm, yyNO_MATCH)
              else yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch) = (case (yygetc(strm))
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
fun yyQ85 (strm, lastMatch) = (case (yygetc(strm))
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
            if inp = #","
              then yyQ75(strm', lastMatch)
            else if inp < #","
              then if inp = #"#"
                  then yyQ80(strm', lastMatch)
                else if inp < #"#"
                  then if inp = #"\r"
                      then yyQ84(strm', lastMatch)
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ36(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ85(strm', lastMatch)
                              else yyQ80(strm', lastMatch)
                          else yyQ85(strm', lastMatch)
                    else if inp = #"!"
                      then yyQ80(strm', lastMatch)
                    else if inp < #"!"
                      then if inp = #" "
                          then yyQ85(strm', lastMatch)
                          else yyQ80(strm', lastMatch)
                      else yyQ78(strm', lastMatch)
                else if inp = #"("
                  then yyQ69(strm', lastMatch)
                else if inp < #"("
                  then if inp = #"&"
                      then yyQ62(strm', lastMatch)
                    else if inp < #"&"
                      then if inp = #"$"
                          then yyQ64(strm', lastMatch)
                          else yyQ83(strm', lastMatch)
                      else yyQ80(strm', lastMatch)
                else if inp = #"*"
                  then yyQ66(strm', lastMatch)
                else if inp = #")"
                  then yyQ70(strm', lastMatch)
                  else yyQ65(strm', lastMatch)
            else if inp = #"@"
              then yyQ80(strm', lastMatch)
            else if inp < #"@"
              then if inp = #";"
                  then yyQ68(strm', lastMatch)
                else if inp < #";"
                  then if inp = #"/"
                      then yyQ76(strm', lastMatch)
                    else if inp < #"/"
                      then if inp = #"-"
                          then yyQ80(strm', lastMatch)
                          else yyQ63(strm', lastMatch)
                      else yyQ80(strm', lastMatch)
                else if inp = #">"
                  then yyQ74(strm', lastMatch)
                else if inp < #">"
                  then if inp = #"<"
                      then yyQ73(strm', lastMatch)
                      else yyQ77(strm', lastMatch)
                  else yyQ67(strm', lastMatch)
            else if inp = #"^"
              then yyQ79(strm', lastMatch)
            else if inp < #"^"
              then if inp = #"\\"
                  then yyQ81(strm', lastMatch)
                else if inp < #"\\"
                  then if inp = #"["
                      then yyQ71(strm', lastMatch)
                      else yyQ80(strm', lastMatch)
                  else yyQ72(strm', lastMatch)
            else if inp = #"|"
              then yyQ61(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"{"
                  then yyQ82(strm', lastMatch)
                  else yyQ80(strm', lastMatch)
              else yyQ80(strm', lastMatch)
      (* end case *))
fun yyQ51 (strm, lastMatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ52 (strm, lastMatch) = yyAction38(strm, yyNO_MATCH)
fun yyQ53 (strm, lastMatch) = yyAction40(strm, yyNO_MATCH)
fun yyQ54 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ55 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction41(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction41(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction41(strm, yyNO_MATCH)
                      else yyQ59(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction41(strm, yyNO_MATCH)
                  else yyQ58(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
            else if inp = #"a"
              then yyQ58(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"\\"
                  then yyQ58(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                  else yyAction41(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ58(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch) = (case (yygetc(strm))
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
fun yyQ57 (strm, lastMatch) = (case (yygetc(strm))
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
              then yyQ54(strm', lastMatch)
            else if inp < #"!"
              then if inp = #"\v"
                  then yyQ57(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\t"
                      then yyQ57(strm', lastMatch)
                    else if inp = #"\n"
                      then yyQ36(strm', lastMatch)
                      else yyQ54(strm', lastMatch)
                else if inp = #"\^N"
                  then yyQ54(strm', lastMatch)
                else if inp < #"\^N"
                  then if inp = #"\r"
                      then yyQ56(strm', lastMatch)
                      else yyQ57(strm', lastMatch)
                else if inp = #" "
                  then yyQ57(strm', lastMatch)
                  else yyQ54(strm', lastMatch)
            else if inp = #"\\"
              then yyQ55(strm', lastMatch)
            else if inp < #"\\"
              then if inp = #"-"
                  then yyQ52(strm', lastMatch)
                  else yyQ54(strm', lastMatch)
            else if inp = #"^"
              then yyQ51(strm', lastMatch)
            else if inp = #"]"
              then yyQ53(strm', lastMatch)
              else yyQ54(strm', lastMatch)
      (* end case *))
fun yyQ28 (strm, lastMatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ29 (strm, lastMatch) = yyAction16(strm, yyMATCH(strm, yyAction58, yyNO_MATCH))
fun yyQ46 (strm, lastMatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ47 (strm, lastMatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ45 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"8"
              then yyQ47(strm', lastMatch)
            else if inp < #"8"
              then if inp = #"7"
                  then yyQ46(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ50 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"I"
              then yyQ45(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"I"
              then yyQ50(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ48 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"C"
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ30 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction58, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"S"
              then yyQ48(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction58, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction58, yyNO_MATCH))
      (* end case *))
fun yyQ44 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ45(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ44(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ42 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ43(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ31 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction58, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ42(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction58, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction58, yyNO_MATCH))
      (* end case *))
fun yyQ40 (strm, lastMatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ39 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"8"
              then yyQ40(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ41 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"F"
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ32 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction58, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"T"
              then yyQ41(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction58, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction58, yyNO_MATCH))
      (* end case *))
fun yyQ38 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"f"
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction58, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ38(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction58, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction58, yyNO_MATCH))
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
            else if inp = #"V"
              then yyQ29(strm', lastMatch)
            else if inp < #"V"
              then if inp = #"B"
                  then yyQ29(strm', lastMatch)
                else if inp < #"B"
                  then if inp = #"A"
                      then yyQ30(strm', lastMatch)
                      else yyQ29(strm', lastMatch)
                else if inp = #"U"
                  then yyQ32(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"b"
              then yyQ29(strm', lastMatch)
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ31(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"u"
              then yyQ33(strm', lastMatch)
              else yyQ29(strm', lastMatch)
      (* end case *))
fun yyQ20 (strm, lastMatch) = yyAction51(strm, yyNO_MATCH)
fun yyQ21 (strm, lastMatch) = yyAction52(strm, yyNO_MATCH)
fun yyQ26 (strm, lastMatch) = yyAction54(strm, yyNO_MATCH)
fun yyQ27 (strm, lastMatch) = yyAction56(strm, yyNO_MATCH)
fun yyQ22 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ27(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"\\"
              then yyQ26(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyAction55(strm, yyNO_MATCH)
                      else yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                else if inp = #"\r"
                  then yyAction55(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp = #"#"
              then yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyAction55(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction55(strm, yyNO_MATCH)
              else yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
      (* end case *))
fun yyQ23 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyAction55(strm, yyNO_MATCH)
                      else yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                else if inp = #"\r"
                  then yyAction55(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp = #"#"
              then yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyAction55(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction55(strm, yyNO_MATCH)
              else yyQ25(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
      (* end case *))
fun yyQ24 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ21(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ23(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ23(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ21(strm', lastMatch)
                      else yyQ23(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ24(strm', lastMatch)
                  else yyQ23(strm', lastMatch)
            else if inp = #"#"
              then yyQ23(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ20(strm', lastMatch)
                  else yyQ23(strm', lastMatch)
            else if inp = #"\\"
              then yyQ22(strm', lastMatch)
              else yyQ23(strm', lastMatch)
      (* end case *))
fun yyQ14 (strm, lastMatch) = yyAction48(strm, yyNO_MATCH)
fun yyQ15 (strm, lastMatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ17 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ17(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp = #"("
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"("
              then yyQ17(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #")"
              then yyAction50(strm, yyNO_MATCH)
              else yyQ17(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
      (* end case *))
fun yyQ16 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ17(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyAction50(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp = #"("
              then yyAction50(strm, yyNO_MATCH)
            else if inp < #"("
              then yyQ17(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp <= #")"
              then yyAction50(strm, yyNO_MATCH)
              else yyQ17(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
      (* end case *))
fun yyQ19 (strm, lastMatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ18 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ19(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
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
              else yyQ16(strm', lastMatch)
      (* end case *))
fun yyQ7 (strm, lastMatch) = yyAction46(strm, yyNO_MATCH)
fun yyQ8 (strm, lastMatch) = yyAction46(strm, yyNO_MATCH)
fun yyQ9 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ8(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch) = yyAction45(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ13(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ11 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ12(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ7(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ7(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ8(strm', lastMatch)
                      else yyQ7(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ9(strm', lastMatch)
                  else yyQ7(strm', lastMatch)
            else if inp = #")"
              then yyQ7(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ11(strm', lastMatch)
                  else yyQ7(strm', lastMatch)
            else if inp = #"*"
              then yyQ10(strm', lastMatch)
              else yyQ7(strm', lastMatch)
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
	    handle IO.Io{cause, ...} => raise cause
          end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
