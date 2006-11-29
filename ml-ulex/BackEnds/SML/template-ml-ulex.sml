@header@
  = struct

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
@startstates@

    structure UserDeclarations = 
      struct

@userdecls@


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
@lexer@

            and skip() = (yystartPos := yygetPos(); continue())
	    in (continue(), (!yystartPos, yygetPos()), !yystrm, !yyss) end
          in 
            lex()
          end

    type pos = int
    type span = pos * pos
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of yyInput.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm (STRM (yystrm, memo), ss) = (case !memo
	  of NONE => (let
	     val (tok, span, yystrm', ss') = innerLex (yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
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
		 lex sm (STRM (yystrm, memo), ss))
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

    fun streamify inputN = (STRM (yyInput.mkStream inputN, ref NONE), 
			    INITIAL)

    fun mkSourcemap() = ref []

(*    fun getLineNo (STRM (strm, _), _) = yyInput.getline strm *)
    fun getPos (STRM (strm, _), _) = yyInput.getpos strm
    fun getLineNo sm pos = yylineNo (sm, pos)
    fun getColNo  sm pos = yycolNo (sm, pos)

  end
