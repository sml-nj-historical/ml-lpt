structure CalcLex  = struct

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

    datatype 'a yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * 'a action * 'a yymatch
    withtype 'a action = yyInput.stream * 'a yymatch -> 'a

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

 
  open Tok
  type lex_result = token


      end

    exception yyEOF

    type yysourcemap = (int * int) list ref
    fun yyfindLB ((lineNo, pos)::sm, pos') = 
	  if pos <= pos' then (lineNo, pos)
	  else yyfindLB(sm, pos')
      | yyfindLB _ = raise Fail "BUG: yyfindLB"
    fun yylineNo (sm, pos) = #1 (yyfindLB(!sm, pos))
    fun yycolNo  (sm, pos) = pos - (#2 (yyfindLB(!sm, pos))) + 1

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
fun yyAction0 (strm, lastMatch) = (yystrm := strm;   KW_let )
fun yyAction1 (strm, lastMatch) = (yystrm := strm;   KW_in )
fun yyAction2 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   ID (yytext) 
      end
fun yyAction3 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   NUM (valOf (Int.fromString (yytext))) 
      end
fun yyAction4 (strm, lastMatch) = (yystrm := strm;   EQ )
fun yyAction5 (strm, lastMatch) = (yystrm := strm;   PLUS )
fun yyAction6 (strm, lastMatch) = (yystrm := strm;   MINUS )
fun yyAction7 (strm, lastMatch) = (yystrm := strm;   TIMES )
fun yyAction8 (strm, lastMatch) = (yystrm := strm;   LP )
fun yyAction9 (strm, lastMatch) = (yystrm := strm;   RP )
fun yyAction10 (strm, lastMatch) = (yystrm := strm;   SEMI )
fun yyAction11 (strm, lastMatch) = (yystrm := strm;   continue() )
fun yyQ1 (strm, lastMatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ2 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ3 (strm, lastMatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ4 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ5 (strm, lastMatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ6 (strm, lastMatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ7 (strm, lastMatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ8 (strm, lastMatch) = yyAction11(strm, yyNO_MATCH)
fun yyQ9 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ9(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction3(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ9(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction1(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx6E
              then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp <= 0wx60
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ14(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx74
              then if inp <= 0wx60
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch) = (case (yygetc(strm))
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
                      else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx65
              then yyQ13(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx65
              then if inp <= 0wx60
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => if yyInput.eof(strm)
              then raise yyEOF
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then if yyInput.eof(strm)
                  then raise yyEOF
                  else yystuck(lastMatch)
            else if inp < 0wx3A
              then if inp = 0wx29
                  then yyQ6(strm', lastMatch)
                else if inp < 0wx29
                  then if inp = 0wx20
                      then yyQ8(strm', lastMatch)
                    else if inp < 0wx20
                      then if inp = 0wx9
                          then yyQ8(strm', lastMatch)
                        else if inp < 0wx9
                          then if yyInput.eof(strm)
                              then raise yyEOF
                              else yystuck(lastMatch)
                        else if inp <= 0wxA
                          then yyQ8(strm', lastMatch)
                        else if yyInput.eof(strm)
                          then raise yyEOF
                          else yystuck(lastMatch)
                    else if inp = 0wx28
                      then yyQ5(strm', lastMatch)
                    else if yyInput.eof(strm)
                      then raise yyEOF
                      else yystuck(lastMatch)
                else if inp = 0wx2D
                  then yyQ3(strm', lastMatch)
                else if inp < 0wx2D
                  then if inp = 0wx2B
                      then yyQ2(strm', lastMatch)
                    else if inp = 0wx2A
                      then yyQ4(strm', lastMatch)
                    else if yyInput.eof(strm)
                      then raise yyEOF
                      else yystuck(lastMatch)
                else if inp <= 0wx2F
                  then if yyInput.eof(strm)
                      then raise yyEOF
                      else yystuck(lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = 0wx61
              then yyQ10(strm', lastMatch)
            else if inp < 0wx61
              then if inp = 0wx3E
                  then if yyInput.eof(strm)
                      then raise yyEOF
                      else yystuck(lastMatch)
                else if inp < 0wx3E
                  then if inp = 0wx3C
                      then if yyInput.eof(strm)
                          then raise yyEOF
                          else yystuck(lastMatch)
                    else if inp = 0wx3B
                      then yyQ7(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = 0wx41
                  then yyQ10(strm', lastMatch)
                else if inp < 0wx41
                  then if yyInput.eof(strm)
                      then raise yyEOF
                      else yystuck(lastMatch)
                else if inp <= 0wx5A
                  then yyQ10(strm', lastMatch)
                else if yyInput.eof(strm)
                  then raise yyEOF
                  else yystuck(lastMatch)
            else if inp = 0wx6C
              then yyQ12(strm', lastMatch)
            else if inp < 0wx6C
              then if inp = 0wx69
                  then yyQ11(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ10(strm', lastMatch)
            else if yyInput.eof(strm)
              then raise yyEOF
              else yystuck(lastMatch)
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

    fun streamify inputN = (STRM (yyInput.mkStream inputN, ref [(1, 0)], ref NONE), 
			    INITIAL)

(*    fun getLineNo (STRM (strm, _), _) = yyInput.getline strm *)
    fun getPos (STRM (strm, _, _), _) = yyInput.getpos strm
    fun getLineNo ((STRM (_, sm, _), _), pos) = yylineNo (sm, pos)
    fun getColNo  ((STRM (_, sm, _), _), pos) = yycolNo (sm, pos)

  end
