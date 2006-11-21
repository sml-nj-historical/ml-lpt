structure Mlex  = struct

(* utf8.sml
 *
 * COPYRIGHT (c) 2004 The Moby Project (moby.cs.uchicago.edu)
 * All rights reserved.
 * USED WITH PERMISSION
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

    structure yyUTF8 = struct

      structure W = Word32
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

      fun getList getc strm = let
	    val get1 = getu getc
	    fun iter (strm, accum) = (case get1 strm
		  of NONE => rev accum
		   | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
            in
	      iter (strm, [])
            end

    end

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val subtract : stream * stream -> Substring.substring
	val eof : stream -> bool

      end = struct

        val chunkSize = 4096

        datatype stream = S of (buf * int) 
	and buf = B of {
	    data : string,
	    basePos : int,
	    more : more ref,
	    inputN : int -> string
          }
	and more = UNKNOWN | YES of buf | NO
        
	fun mkStream inputN = 
	      (S (B {data = "", basePos = 0, 
		     more = ref UNKNOWN,
		     inputN = inputN},
		  0))

	fun getc (S (buf as B {data, basePos, more, inputN}, pos)) = 
	      if pos < String.size data then let
		val c = String.sub (data, pos)
		in
		  SOME (c, S (buf, pos+1))
		end
	      else (case !more
		     of NO => NONE
		      | YES buf' => getc (S (buf', 0))
		      | UNKNOWN => 
			  (case inputN chunkSize
			    of "" => (more := NO; NONE)
			     | data' => let 
				 val buf' = B {data = data',
					       basePos = basePos + 
						 String.size data,
					       more = ref UNKNOWN,
					       inputN = inputN}
			         in
				   more := YES buf';
				   getc (S (buf', 0))
			         end
			   (* end case *))
		    (* end case *))

	fun getpos (S (B {basePos, ...}, pos)) = basePos + pos

	fun subtract (new, old) = let
	      val (S (B {data = ndata, basePos = nbasePos, ...}, npos)) = new
	      val (S (B {data = odata, basePos = obasePos, 
			 more, inputN}, opos)) = old
	      in
	        if nbasePos = obasePos then
		  Substring.substring (ndata, opos, npos-opos)
		else case !more
		      of NO =>      raise Fail "BUG: yyInput.subtract, but buffers are unrelated"
		       | UNKNOWN => raise Fail "BUG: yyInput.subtract, but buffers are unrelated"
		       | YES buf => 
			   Substring.extract (
			     Substring.concat [
			       Substring.extract (odata, opos, NONE),
			       subtract (new, S (buf, 0))],
			     0, NONE)
	      end

	fun eof (S (B {data, more, ...}, pos)) = 
	      pos >= String.size data andalso 
	      (case !more
		of NO => true
		 | _ => false)

      end

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

 
  open Tok;
  fun eof() = EOF
  type lex_result = Tok.token


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lex_result

    exception yyEOF

    type yysourcemap = (int * int) list ref
    fun yyfindLB ((lineNo, pos)::sm, pos') = 
	  if pos <= pos' then (lineNo, pos)
	  else yyfindLB(sm, pos')
      | yyfindLB _ = (1, ~1)
    fun yylineNo (sm, pos) = #1 (yyfindLB(!sm, pos))
    fun yycolNo  (sm, pos) = pos - (#2 (yyfindLB(!sm, pos))) 

    fun innerLex (yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yygetPos() = yyInput.getpos (!yystrm)
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case yyUTF8.getu yyInput.getc strm
                of (SOME (0w10, s')) => let
	             val (curLine, pos) = case !yysm
					   of x::_ => x
					    | nil => (1, ~1)
		     val newPos = yyInput.getpos strm
	             in
	               (if pos < newPos then
		          yysm := (curLine + 1, newPos)::(!yysm)
			else ());
		       SOME (0w10, s')
	             end
		 | x => x)
	(* create yytext *)
	  fun yymksubstr(strm) = yyInput.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yyUTF8.getList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = yylineNo (yysm, yyInput.getpos strm)
	    fun continue() = 
let
fun yyAction0 (strm, lastMatch) = (yystrm := strm;  continue())
fun yyAction1 (strm, lastMatch) = (yystrm := strm;  continue())
fun yyAction2 (strm, lastMatch) = (yystrm := strm;  KW_program)
fun yyAction3 (strm, lastMatch) = (yystrm := strm;  LP)
fun yyAction4 (strm, lastMatch) = (yystrm := strm;  RP)
fun yyAction5 (strm, lastMatch) = (yystrm := strm;  SEMI)
fun yyAction6 (strm, lastMatch) = (yystrm := strm;  DOT)
fun yyAction7 (strm, lastMatch) = (yystrm := strm;  COMMA)
fun yyAction8 (strm, lastMatch) = (yystrm := strm;  KW_var)
fun yyAction9 (strm, lastMatch) = (yystrm := strm;  COLON)
fun yyAction10 (strm, lastMatch) = (yystrm := strm;  KW_array)
fun yyAction11 (strm, lastMatch) = (yystrm := strm;  LSB)
fun yyAction12 (strm, lastMatch) = (yystrm := strm;  RSB)
fun yyAction13 (strm, lastMatch) = (yystrm := strm;  KW_of)
fun yyAction14 (strm, lastMatch) = (yystrm := strm;  KW_integer)
fun yyAction15 (strm, lastMatch) = (yystrm := strm;  KW_real)
fun yyAction16 (strm, lastMatch) = (yystrm := strm;  KW_function)
fun yyAction17 (strm, lastMatch) = (yystrm := strm;  KW_procedure)
fun yyAction18 (strm, lastMatch) = (yystrm := strm;  KW_begin)
fun yyAction19 (strm, lastMatch) = (yystrm := strm;  KW_end)
fun yyAction20 (strm, lastMatch) = (yystrm := strm;  ASSIGNOP)
fun yyAction21 (strm, lastMatch) = (yystrm := strm;  KW_if)
fun yyAction22 (strm, lastMatch) = (yystrm := strm;  KW_then)
fun yyAction23 (strm, lastMatch) = (yystrm := strm;  KW_else)
fun yyAction24 (strm, lastMatch) = (yystrm := strm;  KW_while)
fun yyAction25 (strm, lastMatch) = (yystrm := strm;  KW_do)
fun yyAction26 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  RELOP yytext
      end
fun yyAction27 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  ADDOP yytext
      end
fun yyAction28 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  MULOP yytext
      end
fun yyAction29 (strm, lastMatch) = (yystrm := strm;  KW_not)
fun yyAction30 (strm, lastMatch) = (yystrm := strm;  MINUS)
fun yyAction31 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  ID yytext
      end
fun yyAction32 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  INT (valOf (IntInf.fromString yytext))
      end
fun yyAction33 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         print (concat ["Unexpected character: '", yytext,
			           "'\n"]); continue()
      end
fun yyQ1 (strm, lastMatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ2 (strm, lastMatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ3 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ4 (strm, lastMatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ5 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ101 (strm, lastMatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ6 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ101(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch) = yyAction11(strm, yyNO_MATCH)
fun yyQ8 (strm, lastMatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ9 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ11 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ12 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ13 (strm, lastMatch) = yyAction33(strm, yyNO_MATCH)
fun yyQ100 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ100(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction32(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ100(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ100(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction32(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ100(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction29(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction29(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction29(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ99(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ98(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ59(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ54(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ18 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ97(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ97(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx3D
              then yyAction26(strm, yyNO_MATCH)
            else if inp <= 0wx3E
              then yyQ97(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction25(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction25(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction25(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction25(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction25(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ59(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ95(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx69
                  then yyQ96(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction24(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction24(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction24(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction24(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction24(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ94(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6C
              then yyQ93(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ92(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ91(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction22(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction22(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction22(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction22(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction22(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                  else yyAction22(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ90(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ88(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction23(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction23(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ87(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ86(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction19(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction19(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction19(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ85(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ84(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx6C
                  then yyQ83(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction18(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction18(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction18(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction18(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ82(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ81(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ80(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ79(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction16(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction16(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction16(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction16(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction16(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ78(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ77(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ76(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ75(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx63
              then yyQ74(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx63
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ72(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction15(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction15(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction15(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction15(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6C
              then yyQ71(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ70(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ69(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction21(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction21(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction21(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction21(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction14(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction14(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction14(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction14(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ68(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ67(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ66(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ65(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ64(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ63(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx66
                  then yyQ62(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction13(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction13(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction13(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction13(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction13(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction27(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction27(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ61(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx66
                  then yyQ60(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction10(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction10(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx79
              then yyQ58(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx79
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ57(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ56(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ55(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx6E
                  then yyQ54(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ53(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ51(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ50(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ49(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ48(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ47(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ46(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ45(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ44(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ43(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx63
                  then yyQ42(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ41(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ40(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ38 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx7D
              then yyQ37(strm', lastMatch)
              else yyQ38(strm', lastMatch)
      (* end case *))
fun yyQ32 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx7D
              then yyQ37(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyQ38(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
      (* end case *))
fun yyQ36 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
and yyQ35 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => if yyInput.eof(strm)
              then raise yyEOF
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx5C
              then yyQ13(strm', lastMatch)
            else if inp < 0wx5C
              then if inp = 0wx2C
                  then yyQ5(strm', lastMatch)
                else if inp < 0wx2C
                  then if inp = 0wx20
                      then yyQ34(strm', lastMatch)
                    else if inp < 0wx20
                      then if inp = 0wxB
                          then yyQ34(strm', lastMatch)
                        else if inp < 0wxB
                          then if inp = 0wx9
                              then yyQ34(strm', lastMatch)
                            else if inp = 0wxA
                              then yyQ35(strm', lastMatch)
                              else yyQ13(strm', lastMatch)
                        else if inp = 0wxD
                          then yyQ33(strm', lastMatch)
                        else if inp <= 0wxC
                          then yyQ34(strm', lastMatch)
                          else yyQ13(strm', lastMatch)
                    else if inp = 0wx29
                      then yyQ2(strm', lastMatch)
                    else if inp < 0wx29
                      then if inp = 0wx28
                          then yyQ1(strm', lastMatch)
                          else yyQ13(strm', lastMatch)
                    else if inp = 0wx2A
                      then yyQ12(strm', lastMatch)
                      else yyQ11(strm', lastMatch)
                else if inp = 0wx3C
                  then yyQ19(strm', lastMatch)
                else if inp < 0wx3C
                  then if inp = 0wx30
                      then yyQ14(strm', lastMatch)
                    else if inp < 0wx30
                      then if inp = 0wx2E
                          then yyQ4(strm', lastMatch)
                        else if inp = 0wx2D
                          then yyQ10(strm', lastMatch)
                          else yyQ12(strm', lastMatch)
                    else if inp = 0wx3A
                      then yyQ6(strm', lastMatch)
                    else if inp = 0wx3B
                      then yyQ3(strm', lastMatch)
                      else yyQ14(strm', lastMatch)
                else if inp = 0wx3F
                  then yyQ13(strm', lastMatch)
                else if inp < 0wx3F
                  then if inp = 0wx3D
                      then yyQ9(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx41
                  then yyQ15(strm', lastMatch)
                else if inp < 0wx41
                  then yyQ13(strm', lastMatch)
                else if inp = 0wx5B
                  then yyQ7(strm', lastMatch)
                  else yyQ15(strm', lastMatch)
            else if inp = 0wx6E
              then yyQ16(strm', lastMatch)
            else if inp < 0wx6E
              then if inp = 0wx65
                  then yyQ23(strm', lastMatch)
                else if inp < 0wx65
                  then if inp = 0wx62
                      then yyQ24(strm', lastMatch)
                    else if inp < 0wx62
                      then if inp = 0wx5E
                          then yyQ13(strm', lastMatch)
                        else if inp < 0wx5E
                          then yyQ8(strm', lastMatch)
                        else if inp = 0wx61
                          then yyQ29(strm', lastMatch)
                          else yyQ13(strm', lastMatch)
                    else if inp = 0wx63
                      then yyQ15(strm', lastMatch)
                      else yyQ20(strm', lastMatch)
                else if inp = 0wx69
                  then yyQ27(strm', lastMatch)
                else if inp < 0wx69
                  then if inp = 0wx66
                      then yyQ25(strm', lastMatch)
                      else yyQ15(strm', lastMatch)
                else if inp = 0wx6D
                  then yyQ17(strm', lastMatch)
                  else yyQ15(strm', lastMatch)
            else if inp = 0wx75
              then yyQ15(strm', lastMatch)
            else if inp < 0wx75
              then if inp = 0wx72
                  then yyQ26(strm', lastMatch)
                else if inp < 0wx72
                  then if inp = 0wx70
                      then yyQ31(strm', lastMatch)
                    else if inp = 0wx6F
                      then yyQ28(strm', lastMatch)
                      else yyQ15(strm', lastMatch)
                else if inp = 0wx73
                  then yyQ15(strm', lastMatch)
                  else yyQ22(strm', lastMatch)
            else if inp = 0wx78
              then yyQ15(strm', lastMatch)
            else if inp < 0wx78
              then if inp = 0wx76
                  then yyQ30(strm', lastMatch)
                  else yyQ21(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ32(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ15(strm', lastMatch)
              else yyQ13(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            and skip() = (yystartPos := yygetPos(); continue())
	    in (continue(), (!yystartPos, yygetPos()), !yystrm, !yyss) end
          in 
            lex()
          end

    type pos = int
    type span = pos * pos
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of yyInput.stream * yysourcemap *
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex(STRM (yystrm, sm, memo), ss) = (case !memo
	  of NONE => (let
	     val (tok, span, yystrm', ss') = innerLex (yystrm, ss, sm)
	     val strm' = STRM (yystrm', sm, ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       SOME (tok, span, (strm', ss'))
	     end
	     handle yyEOF => NONE)
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 SOME (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex (STRM (yystrm, sm, memo), ss))
         (* end case *))

(*	  
    fun lex(STRM (ref(EVAL t))) = SOME t
      | lex(STRM (s as ref(UNEVAL f))) = let
	  val tok = f()
          val t = (tok, STRM(ref(UNEVAL f))) 
          in
	    s := EVAL t; 
	    SOME(t)
          end
	  handle yyEOF => NONE

    fun streamify inputN = STRM(ref(UNEVAL (mk (yyInput.mkStream inputN))))
*)
(*    fun cons(a,s) = STRM(ref(EVAL(a,s))) *)

    fun streamify inputN = (STRM (yyInput.mkStream inputN, ref [], ref NONE), 
			    INITIAL)

(*    fun getLineNo (STRM (strm, _), _) = yyInput.getline strm *)
    fun getPos (STRM (strm, _, _), _) = yyInput.getpos strm
    fun getLineNo ((STRM (_, sm, _), _), pos) = yylineNo (sm, pos)
    fun getColNo  ((STRM (_, sm, _), _), pos) = yycolNo (sm, pos)

  end
