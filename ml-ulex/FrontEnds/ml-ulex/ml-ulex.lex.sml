(* utf8.sml
 *
 * COPYRIGHT (c) 2004 The Moby Project (moby.cs.uchicago.edu)
 * All rights reserved.
 *
 * Routines for working with UTF8 encoded strings.
 *
 *	Unicode value		1st byte    2nd byte    3rd byte    4th byte
 *	-----------------	--------    --------    --------    --------
 *	00000000 0xxxxxxx	0xxxxxxx	
 *	00000yyy yyxxxxxx	110yyyyy    10xxxxxx
 *	zzzzyyyy yyxxxxxx	1110zzzz    10yyyyyy	10xxxxxx
 *	110110ww wwzzzzyy+
 *	110111yy yyxxxxxx	11110uuu    10uuzzzz	10yyyyyy    10xxxxxx!
 *
 * (!) where uuuuu = wwww+1
 *
 * TODO:
 *    Add support for surrogate pairs (this will require changing the type
 *    of wchar to Word32.word).
 *)

structure UTF8 =
  struct

    structure W = Word
    type wchar = W.word

    exception Incomplete
	(* raised by some operations when applied to incomplete strings. *)

    fun getu getc strm = let
          fun getContByte (strm, wc) = (case getc strm
	        of NONE => raise Incomplete
		 | SOME(c, strm') => let
		     val b = W.fromInt(Char.ord c)
		     in
		       if (W.andb(0wxc0, b) = 0wx80)
		       then (strm', W.orb(W.<<(wc, 0w6), W.andb(0wx3f, b)))
		       else raise Incomplete
		     end
   	       (* end case *))
          in case getc strm
	      of NONE => NONE
	       | SOME(c, strm') => let
		   val w = W.fromInt(Char.ord c)
		   in
		     if (w < 0w128)
		     then SOME (w, strm')
		     else (case (W.andb(0wxe0, w))
			    of 0wxc0 => let
				 val (strm', wc) = getContByte(strm', W.andb(0wx1f, w))
				 in
				   SOME (wc, strm')
			         end
			     | 0wxe0 => let
				 val (strm', wc) =
				       getContByte(
					 getContByte(strm', W.andb(0wx0f, w)))
			         in
				   SOME (wc, strm')
				 end
			     | _ => raise Incomplete
			   (* end case *))
		   end
             (* end case *)
	  end

  end

(***** ADAPTED FROM ML-YACC *****)
structure Streamify =
struct
   datatype str = EVAL of Tok.token * strm | UNEVAL of (unit -> Tok.token)
   and strm = STRM of str ref

   fun lex(STRM (ref(EVAL t))) = SOME t
     | lex(STRM (s as ref(UNEVAL f))) = let
	 val tok = f()
         val t = (tok, STRM(ref(UNEVAL f))) 
         in
	   case tok
	    of Tok.EOF => NONE
	     | _ => (s := EVAL t; SOME(t))
         end

   fun streamify f = STRM(ref(UNEVAL f))
   fun cons(a,s) = STRM(ref(EVAL(a,s)))

end


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

	val initPos = 0 

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

 
  val comLvl : int ref = ref 0		(* nesting depth of comments *)
  val comStart : int ref = ref 0	(* start line of current comment *)

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
	  val yygetc = UTF8.getu yyInput.getc 
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
fun yyAction0 (strm, lastMatch) = (yystrm := strm;  continue())
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
fun yyAction12 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.UNICODE)
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
fun yyAction21 (strm, lastMatch) = (yystrm := strm;  Tok.STAR)
fun yyAction22 (strm, lastMatch) = (yystrm := strm;  Tok.QUERY)
fun yyAction23 (strm, lastMatch) = (yystrm := strm;  Tok.SEMI)
fun yyAction24 (strm, lastMatch) = (yystrm := strm;  Tok.LP)
fun yyAction25 (strm, lastMatch) = (yystrm := strm;  Tok.RP)
fun yyAction26 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CHARCLASS; Tok.LSB)
fun yyAction27 (strm, lastMatch) = (yystrm := strm;  Tok.RSB)
fun yyAction28 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID (chomp yytext)
      end
fun yyAction29 (strm, lastMatch) = (yystrm := strm;  YYBEGIN DIRECTIVE; Tok.LT)
fun yyAction30 (strm, lastMatch) = (yystrm := strm;  Tok.GT)
fun yyAction31 (strm, lastMatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction32 (strm, lastMatch) = (yystrm := strm;  Tok.SLASH)
fun yyAction33 (strm, lastMatch) = (yystrm := strm;  Tok.EQ)
fun yyAction34 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN CODE; clrText(); Tok.DARROW)
fun yyAction35 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN STRING; clrText(); 
		    ignore(continue() before YYBEGIN INITIAL);
		    (Tok.STRING o valOf o String.fromString o getText)())
fun yyAction36 (strm, lastMatch) = (yystrm := strm;  Tok.CARAT)
fun yyAction37 (strm, lastMatch) = (yystrm := strm;  Tok.DASH)
fun yyAction38 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         let val c = Char.fromString yytext
            in case c
                of SOME c' => Tok.CHAR c'
		 | NONE => Tok.CHAR (String.sub (yytext, 1))
            end
      end
fun yyAction39 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Tok.RSB)
fun yyAction40 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.CHAR (String.sub (yytext, 0))
      end
fun yyAction41 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue()
      end
fun yyAction42 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    addText yytext;
	    ignore(continue() before YYBEGIN CODE);
	    continue()
      end
fun yyAction43 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; comLvl := !comLvl+1; continue()
      end
fun yyAction44 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue()
      end
fun yyAction45 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction46 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         if !pcount = 0 then () else addText yytext;
		    inc pcount; continue()
      end
fun yyAction47 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL; Tok.CODE (getText()))
		    else (addText yytext; continue())
      end
fun yyAction48 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    addText "\""; continue()
      end
fun yyAction49 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction50 (strm, lastMatch) = (yystrm := strm;  Tok.BOGUS)
fun yyAction51 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; print ("unclosed string");
 	            Tok.BOGUS
      end
fun yyAction52 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction53 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction54 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction55 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction56 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.CHAR (String.sub (yytext, 0))
      end
fun yyAction57 (strm, lastMatch) = let
      val yylineno = ref(yyInput.getlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         print (concat[Int.toString (!yylineno), ": illegal character '", 
				  String.toCString yytext, "'\n"]);
		    continue()
      end
fun yyQ115 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ116 (strm, lastMatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ117 (strm, lastMatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ118 (strm, lastMatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ119 (strm, lastMatch) = yyAction11(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
fun yyQ122 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ122(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxB
              then if inp <= 0wx8
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ121 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxB
              then if inp <= 0wx8
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ116(strm', lastMatch)
            else if inp < 0wx3B
              then if inp = 0wx20
                  then yyQ121(strm', lastMatch)
                else if inp < 0wx20
                  then if inp = 0wxA
                      then yyQ34(strm', lastMatch)
                    else if inp < 0wxA
                      then if inp = 0wx9
                          then yyQ121(strm', lastMatch)
                          else yyQ119(strm', lastMatch)
                      else yyQ119(strm', lastMatch)
                else if inp = 0wx2C
                  then yyQ115(strm', lastMatch)
                  else yyQ119(strm', lastMatch)
            else if inp = 0wx41
              then yyQ120(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx3E
                  then yyQ118(strm', lastMatch)
                else if inp < 0wx3E
                  then if inp = 0wx3C
                      then yyQ119(strm', lastMatch)
                      else yyQ117(strm', lastMatch)
                  else yyQ119(strm', lastMatch)
            else if inp = 0wx61
              then yyQ120(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ120(strm', lastMatch)
                  else yyQ119(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ120(strm', lastMatch)
              else yyQ119(strm', lastMatch)
      (* end case *))
fun yyQ65 (strm, lastMatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ66 (strm, lastMatch) = yyAction18(strm, yyNO_MATCH)
fun yyQ67 (strm, lastMatch) = yyAction19(strm, yyNO_MATCH)
fun yyQ68 (strm, lastMatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ69 (strm, lastMatch) = yyAction21(strm, yyNO_MATCH)
fun yyQ70 (strm, lastMatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ71 (strm, lastMatch) = yyAction23(strm, yyNO_MATCH)
fun yyQ114 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ72 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ114(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch) = yyAction25(strm, yyNO_MATCH)
fun yyQ74 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ75 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ76 (strm, lastMatch) = yyAction29(strm, yyNO_MATCH)
fun yyQ77 (strm, lastMatch) = yyAction30(strm, yyNO_MATCH)
fun yyQ78 (strm, lastMatch) = yyAction31(strm, yyNO_MATCH)
fun yyQ79 (strm, lastMatch) = yyAction32(strm, yyNO_MATCH)
fun yyQ113 (strm, lastMatch) = yyAction34(strm, yyNO_MATCH)
fun yyQ80 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ113(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch) = yyAction35(strm, yyNO_MATCH)
fun yyQ82 (strm, lastMatch) = yyAction56(strm, yyNO_MATCH)
fun yyQ61 (strm, lastMatch) = yyAction38(strm, yyNO_MATCH)
fun yyQ64 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx7D
              then yyQ61(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ63 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx33
              then yyQ64(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ62 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx7B
              then yyQ63(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ83 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction56(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction56(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction56(strm, yyNO_MATCH)
                      else yyQ62(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ61(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ61(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5C
                  then yyQ61(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ61(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ111 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ111(strm', lastMatch)
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yystuck(lastMatch)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yystuck(lastMatch)
                      else yyQ111(strm', lastMatch)
                else if inp = 0wx41
                  then yyQ111(strm', lastMatch)
                else if inp < 0wx41
                  then yystuck(lastMatch)
                else if inp <= 0wx5A
                  then yyQ111(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx7B
              then yystuck(lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx60
                  then yystuck(lastMatch)
                  else yyQ111(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ112(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ84 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction56(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp <= 0wx40
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ111(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ111(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction56(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ111(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ109 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ110(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ108 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ109(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ107 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ108(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ106 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ107(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ105 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ106(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ87 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx68
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ104 (strm, lastMatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ103 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ104(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ88 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ103(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ102 (strm, lastMatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ101 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ102(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ100 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ101(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ99 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ100(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ98 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ99(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ89 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ98(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ97 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ96 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ97(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ95 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyQ96(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ90 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ95(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ94 (strm, lastMatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ93 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ94(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ92 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ93(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ91 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ92(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ85 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx6D
              then yyAction56(strm, yyNO_MATCH)
            else if inp < 0wx6D
              then if inp = 0wx64
                  then yyQ91(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp < 0wx64
                  then if inp = 0wx63
                      then yyQ87(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                      else yyAction56(strm, yyNO_MATCH)
                else if inp = 0wx6C
                  then yyQ88(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx6E
                  then yyQ90(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxB
              then if inp <= 0wx8
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ82(strm', lastMatch)
            else if inp < 0wx2D
              then if inp = 0wx24
                  then yyQ67(strm', lastMatch)
                else if inp < 0wx24
                  then if inp = 0wx20
                      then yyQ86(strm', lastMatch)
                    else if inp < 0wx20
                      then if inp = 0wxA
                          then yyQ34(strm', lastMatch)
                        else if inp < 0wxA
                          then if inp = 0wx9
                              then yyQ86(strm', lastMatch)
                              else yyQ82(strm', lastMatch)
                          else yyQ82(strm', lastMatch)
                    else if inp = 0wx22
                      then yyQ81(strm', lastMatch)
                      else yyQ82(strm', lastMatch)
                else if inp = 0wx29
                  then yyQ73(strm', lastMatch)
                else if inp < 0wx29
                  then if inp = 0wx26
                      then yyQ82(strm', lastMatch)
                    else if inp < 0wx26
                      then yyQ85(strm', lastMatch)
                    else if inp = 0wx28
                      then yyQ72(strm', lastMatch)
                      else yyQ82(strm', lastMatch)
                else if inp = 0wx2B
                  then yyQ68(strm', lastMatch)
                else if inp = 0wx2A
                  then yyQ69(strm', lastMatch)
                  else yyQ78(strm', lastMatch)
            else if inp = 0wx40
              then yyQ82(strm', lastMatch)
            else if inp < 0wx40
              then if inp = 0wx3C
                  then yyQ76(strm', lastMatch)
                else if inp < 0wx3C
                  then if inp = 0wx30
                      then yyQ82(strm', lastMatch)
                    else if inp < 0wx30
                      then if inp = 0wx2E
                          then yyQ66(strm', lastMatch)
                          else yyQ79(strm', lastMatch)
                    else if inp = 0wx3B
                      then yyQ71(strm', lastMatch)
                      else yyQ82(strm', lastMatch)
                else if inp = 0wx3E
                  then yyQ77(strm', lastMatch)
                else if inp = 0wx3D
                  then yyQ80(strm', lastMatch)
                  else yyQ70(strm', lastMatch)
            else if inp = 0wx5E
              then yyQ82(strm', lastMatch)
            else if inp < 0wx5E
              then if inp = 0wx5C
                  then yyQ83(strm', lastMatch)
                else if inp < 0wx5C
                  then if inp = 0wx5B
                      then yyQ74(strm', lastMatch)
                      else yyQ82(strm', lastMatch)
                  else yyQ75(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ65(strm', lastMatch)
            else if inp < 0wx7C
              then if inp = 0wx7B
                  then yyQ84(strm', lastMatch)
                  else yyQ82(strm', lastMatch)
              else yyQ82(strm', lastMatch)
      (* end case *))
fun yyQ55 (strm, lastMatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ56 (strm, lastMatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ57 (strm, lastMatch) = yyAction39(strm, yyNO_MATCH)
fun yyQ58 (strm, lastMatch) = yyAction40(strm, yyNO_MATCH)
fun yyQ59 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction40(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction40(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction40(strm, yyNO_MATCH)
                      else yyQ62(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction40(strm, yyNO_MATCH)
                  else yyQ61(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ61(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5C
                  then yyQ61(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                  else yyAction40(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ61(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxB
              then if inp <= 0wx8
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ56(strm', lastMatch)
            else if inp < 0wx2D
              then if inp = 0wxB
                  then yyQ58(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wx9
                      then yyQ60(strm', lastMatch)
                    else if inp = 0wxA
                      then yyQ34(strm', lastMatch)
                      else yyQ58(strm', lastMatch)
                else if inp = 0wx20
                  then yyQ60(strm', lastMatch)
                  else yyQ58(strm', lastMatch)
            else if inp = 0wx5D
              then yyQ57(strm', lastMatch)
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyQ59(strm', lastMatch)
                  else yyQ58(strm', lastMatch)
            else if inp = 0wx5E
              then yyQ55(strm', lastMatch)
              else yyQ58(strm', lastMatch)
      (* end case *))
fun yyQ27 (strm, lastMatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ28 (strm, lastMatch) = yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
fun yyQ50 (strm, lastMatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ51 (strm, lastMatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ49 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx38
              then yyQ51(strm', lastMatch)
            else if inp < 0wx38
              then if inp = 0wx37
                  then yyQ50(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ54 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx49
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ53 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx49
              then yyQ54(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ52 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx43
              then yyQ53(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ29 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx53
              then yyQ52(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction57, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
      (* end case *))
fun yyQ48 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
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
            if inp = 0wx63
              then yyQ47(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ30 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ46(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction57, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
      (* end case *))
fun yyQ40 (strm, lastMatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ45 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx45
              then yyQ40(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx44
              then yyQ45(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx4F
              then yyQ44(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ42 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx43
              then yyQ43(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ41 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx49
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ31 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx4E
              then yyQ41(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction57, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
      (* end case *))
fun yyQ39 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ40(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ38 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx64
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ37 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ38(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ36 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx63
              then yyQ37(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ35 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ36(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ32 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yyQ35(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction57, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction57, yyNO_MATCH))
      (* end case *))
fun yyQ33 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxB
              then if inp <= 0wx8
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ29(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx20
                  then yyQ33(strm', lastMatch)
                else if inp < 0wx20
                  then if inp = 0wxA
                      then yyQ34(strm', lastMatch)
                    else if inp < 0wxA
                      then if inp = 0wx9
                          then yyQ33(strm', lastMatch)
                          else yyQ28(strm', lastMatch)
                      else yyQ28(strm', lastMatch)
                else if inp = 0wx3B
                  then yyQ27(strm', lastMatch)
                  else yyQ28(strm', lastMatch)
            else if inp = 0wx61
              then yyQ30(strm', lastMatch)
            else if inp < 0wx61
              then if inp = 0wx55
                  then yyQ31(strm', lastMatch)
                  else yyQ28(strm', lastMatch)
            else if inp = 0wx75
              then yyQ32(strm', lastMatch)
              else yyQ28(strm', lastMatch)
      (* end case *))
fun yyQ19 (strm, lastMatch) = yyAction50(strm, yyNO_MATCH)
fun yyQ20 (strm, lastMatch) = yyAction51(strm, yyNO_MATCH)
fun yyQ25 (strm, lastMatch) = yyAction53(strm, yyNO_MATCH)
fun yyQ26 (strm, lastMatch) = yyAction54(strm, yyNO_MATCH)
fun yyQ21 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyAction52(strm, yyNO_MATCH)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ26(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                  else yyAction52(strm, yyNO_MATCH)
            else if inp = 0wx5C
              then yyQ25(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch) = yyAction57(strm, yyNO_MATCH)
fun yyQ24 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyAction55(strm, yyNO_MATCH)
            else if inp < 0wx30
              then if inp = 0wxB
                  then yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction55(strm, yyNO_MATCH)
                      else yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                else if inp = 0wx22
                  then yyAction55(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp = 0wx34
              then yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp < 0wx34
              then if inp = 0wx32
                  then yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else yyAction55(strm, yyNO_MATCH)
            else if inp = 0wx5C
              then yyAction55(strm, yyNO_MATCH)
              else yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
      (* end case *))
fun yyQ23 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyAction55(strm, yyNO_MATCH)
            else if inp < 0wx30
              then if inp = 0wxB
                  then yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction55(strm, yyNO_MATCH)
                      else yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                else if inp = 0wx22
                  then yyAction55(strm, yyNO_MATCH)
                  else yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp = 0wx34
              then yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp < 0wx34
              then if inp = 0wx32
                  then yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else yyAction55(strm, yyNO_MATCH)
            else if inp = 0wx5C
              then yyAction55(strm, yyNO_MATCH)
              else yyQ24(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
      (* end case *))
fun yyQ2 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ22(strm', lastMatch)
            else if inp < 0wx30
              then if inp = 0wxB
                  then yyQ23(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ20(strm', lastMatch)
                      else yyQ23(strm', lastMatch)
                else if inp = 0wx22
                  then yyQ19(strm', lastMatch)
                  else yyQ23(strm', lastMatch)
            else if inp = 0wx34
              then yyQ23(strm', lastMatch)
            else if inp < 0wx34
              then if inp = 0wx32
                  then yyQ23(strm', lastMatch)
                  else yyQ22(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ21(strm', lastMatch)
              else yyQ23(strm', lastMatch)
      (* end case *))
fun yyQ13 (strm, lastMatch) = yyAction47(strm, yyNO_MATCH)
fun yyQ14 (strm, lastMatch) = yyAction48(strm, yyNO_MATCH)
fun yyQ16 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ16(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction49(strm, yyNO_MATCH)
                  else yyQ16(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp = 0wx28
              then yyAction49(strm, yyNO_MATCH)
            else if inp < 0wx28
              then yyQ16(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp <= 0wx29
              then yyAction49(strm, yyNO_MATCH)
              else yyQ16(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
      (* end case *))
fun yyQ15 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ16(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction49(strm, yyNO_MATCH)
                  else yyQ16(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp = 0wx28
              then yyAction49(strm, yyNO_MATCH)
            else if inp < 0wx28
              then yyQ16(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp <= 0wx29
              then yyAction49(strm, yyNO_MATCH)
              else yyQ16(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
      (* end case *))
fun yyQ18 (strm, lastMatch) = yyAction42(strm, yyNO_MATCH)
fun yyQ17 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ18(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ15(strm', lastMatch)
            else if inp < 0wx23
              then if inp = 0wxB
                  then yyQ15(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ16(strm', lastMatch)
                      else yyQ15(strm', lastMatch)
                else if inp = 0wx22
                  then yyQ14(strm', lastMatch)
                  else yyQ15(strm', lastMatch)
            else if inp = 0wx29
              then yyQ13(strm', lastMatch)
            else if inp < 0wx29
              then if inp = 0wx28
                  then yyQ17(strm', lastMatch)
                  else yyQ15(strm', lastMatch)
              else yyQ15(strm', lastMatch)
      (* end case *))
fun yyQ7 (strm, lastMatch) = yyAction45(strm, yyNO_MATCH)
fun yyQ8 (strm, lastMatch) = yyAction45(strm, yyNO_MATCH)
fun yyQ12 (strm, lastMatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ9 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx29
              then yyQ12(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ11(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(strm)
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyQ10(strm', lastMatch)
            else if inp < 0wx28
              then if inp = 0wxA
                  then yyQ8(strm', lastMatch)
                  else yyQ7(strm', lastMatch)
            else if inp = 0wx2A
              then yyQ9(strm', lastMatch)
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
    end

  end
